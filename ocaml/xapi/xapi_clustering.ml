(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Cluster_interface

module D=Debug.Make(struct let name="xapi_clustering" end)
open D

(* host-local clustering lock *)
let clustering_lock_m = Mutex.create ()

let with_clustering_lock f =
  debug "Trying to grab host-local clustering lock...";
  Stdext.Threadext.Mutex.execute clustering_lock_m
    (fun () ->
       Stdext.Pervasiveext.finally
         (fun () ->
            debug "Grabbed host-local clustering lock; executing function...";
            f ())
         (fun () -> debug "Function execution finished; returned host-local clustering lock."))

(* Note we have to add type annotations to network/host here because they're only used in the context of
  Db.PIF.get_records_where, and they're just strings there *)
let pif_of_host ~__context (network : API.ref_network) (host : API.ref_host) =
  debug "Looking up PIF for network %s" (Ref.string_of network);
  let pifs = Db.PIF.get_records_where ~__context
      ~expr:Db_filter_types.(And (Eq(Literal (Ref.string_of host),Field "host"),
                                  Eq(Literal (Ref.string_of network),Field "network"))) in
  match pifs with
  | [(ref, record)] ->
    (ref, record)
  | _ ->
    let msg = Printf.sprintf "No PIF found for host:%s and network:%s" (Ref.string_of host) (Ref.string_of network) in
    debug "%s" msg;
    failwith msg

let ip_of_pif (ref,record) =
  let ip = record.API.pIF_IP in
  if ip = "" then failwith (Printf.sprintf "PIF %s does not have any IP" (Ref.string_of ref));
  debug "Got IP %s for PIF %s" ip (Ref.string_of ref);
  Cluster_interface.IPv4 ip

let assert_pif_permaplugged (ref,record) =
  if not record.API.pIF_disallow_unplug then failwith (Printf.sprintf "PIF %s allows unplug" (Ref.string_of ref));
  if not record.pIF_currently_attached then failwith (Printf.sprintf "PIF %s not plugged" (Ref.string_of ref))

let handle_error error =
  (* TODO: replace with API errors? *)
  match error with
  | InternalError message -> failwith ("Internal Error: " ^ message)
  | Unix_error message -> failwith ("Unix Error: " ^ message)

let assert_cluster_host_can_be_created ~__context ~host =
  if Db.Cluster_host.get_refs_where ~__context
      ~expr:Db_filter_types.(Eq(Literal (Ref.string_of host),Field "host")) <> [] then
    failwith "Cluster host cannot be created because it already exists"

let get_sms_of_type_requiring_cluster_stack ~__context ~sr_sm_type ~cluster_stack =
  let sms_matching_sr_type = Db.SM.get_records_where ~__context
      ~expr:Db_filter_types.(Eq(Field "type", Literal sr_sm_type)) in
  List.filter (fun (sm_ref, sm_rec) ->
      List.mem cluster_stack sm_rec.API.sM_required_cluster_stack) sms_matching_sr_type

let find_cluster_host ~__context ~host =
  match Db.Cluster_host.get_refs_where ~__context
          ~expr:(Db_filter_types.(Eq (Field "host", Literal (Ref.string_of host)))) with
  | [ref] -> Some ref
  | _::_  -> (* should never happen; this indicates a bug *)
    let msg = "Multiple cluster_hosts found for host" in
    error "%s %s" msg (Db.Host.get_uuid ~__context ~self:host);
    raise Api_errors.(Server_error(internal_error, [msg; (Ref.string_of host)]))
  | _ -> None

let assert_cluster_host_enabled ~__context ~self ~expected =
  let actual = Db.Cluster_host.get_enabled ~__context ~self in
  if actual <> expected then
    match expected with
    | true  -> raise Api_errors.(Server_error(clustering_disabled, [Ref.string_of self]))
    | false -> raise Api_errors.(Server_error(clustering_enabled, [Ref.string_of self]))

let assert_cluster_host_is_enabled_for_matching_sms ~__context ~host ~sr_sm_type =
  find_cluster_host ~__context ~host
  |> Stdext.Opt.iter (fun cluster_host ->
      let cluster = Db.Cluster_host.get_cluster ~__context ~self:cluster_host in
      let cluster_stack = Db.Cluster.get_cluster_stack ~__context ~self:cluster in
      match get_sms_of_type_requiring_cluster_stack ~__context ~sr_sm_type ~cluster_stack with
      | _::_ ->
        assert_cluster_host_enabled ~__context ~self:cluster_host ~expected:true
      | _ -> ())
