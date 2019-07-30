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

open Test_highlevel
open Cpuid_helpers

module StringOfFeatures = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array

    type output_t = string

    let string_of_input_t = Test_printers.(array int64)

    let string_of_output_t = Test_printers.string
  end

  let transform = Cpuid_helpers.string_of_features

  let tests =
    `QuickAndAutoDocumented
      [
        ([|0L; 2L; 123L|], "00000000-00000002-0000007b")
      ; ([|0L|], "00000000")
      ; ([||], "")
      ]
end)

module FeaturesOfString = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = int64 array

    let string_of_input_t = Test_printers.string

    let string_of_output_t = Test_printers.(array int64)
  end

  let transform = Cpuid_helpers.features_of_string

  let tests =
    `QuickAndAutoDocumented
      [
        ("00000000-00000002-0000007b", [|0L; 2L; 123L|])
      ; ("00000000", [|0L|])
      ; ("", [||])
      ]
end)

module RoundTripFeaturesToFeatures = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array

    type output_t = int64 array

    let string_of_input_t = Test_printers.(array int64)

    let string_of_output_t = Test_printers.(array int64)
  end

  let transform x =
    x |> Cpuid_helpers.string_of_features |> Cpuid_helpers.features_of_string

  let tests =
    `QuickAndAutoDocumented
      (List.map (fun x -> (x, x)) [[|0L; 1L; 123L|]; [|1L|]; [|0L|]; [||]])
end)

module RoundTripStringToString = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = string

    let string_of_input_t = Test_printers.string

    let string_of_output_t = Test_printers.string
  end

  let transform x =
    x |> Cpuid_helpers.features_of_string |> Cpuid_helpers.string_of_features

  let tests =
    `QuickAndAutoDocumented
      (List.map
         (fun x -> (x, x))
         ["00000000-00000002-0000007b"; "00000001"; "00000000"; ""])
end)

module ParseFailure = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = exn

    let string_of_input_t = Test_printers.string

    let string_of_output_t = Test_printers.exn
  end

  exception NoExceptionRaised

  let transform x =
    try
      ignore (Cpuid_helpers.features_of_string x) ;
      raise NoExceptionRaised
    with e -> e

  let tests =
    `QuickAndAutoDocumented
      (List.map
         (fun x -> (x, InvalidFeatureString x))
         ["foo bar baz"; "fgfg-1234"; "0123-foo"; "foo-0123"; "-1234"; "1234-"])
end)

module Extend = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array * int64 array

    type output_t = int64 array

    let string_of_input_t = Test_printers.(pair (array int64) (array int64))

    let string_of_output_t = Test_printers.(array int64)
  end

  let transform (arr0, arr1) = Cpuid_helpers.extend arr0 arr1

  let tests =
    `QuickAndAutoDocumented
      [
        (([||], [||]), [||])
      ; (([||], [|0L; 2L|]), [|0L; 2L|])
      ; (([|1L|], [||]), [||])
      ; (([|1L|], [|0L|]), [|1L|])
      ; (([|1L|], [|0L; 2L|]), [|1L; 2L|])
      ; (([|1L; 0L|], [|0L; 2L|]), [|1L; 0L|])
      ; (([|1L; 0L|], [|0L; 2L; 4L; 9L|]), [|1L; 0L; 4L; 9L|])
      ]
end)

module ZeroExtend = Generic.MakeStateless (struct
    module Io = struct
      type input_t = int64 array * int
      type output_t = int64 array
      let string_of_input_t = Test_printers.(pair (array int64) int)
      let string_of_output_t = Test_printers.(array int64)
    end

    let transform = fun (arr, len) -> Cpuid_helpers.zero_extend arr len

    let tests = `QuickAndAutoDocumented [
      ([| 1L |], 2), [| 1L; 0L |];
      ([| 1L |], 1), [| 1L; |];
      ([| |], 2), [| 0L; 0L |];
      ([| |], 1), [| 0L |];
      ([| |], 0), [| |];
      ([| 1L; 2L |], 0), [| |];
      ([| 1L; 2L |], 1), [| 1L |];
      ([| 1L; 2L |], 2), [| 1L; 2L |];
    ]
  end)


module Accessors = Generic.MakeStateless (struct
    module Io = struct
      type input_t = (string * string) list
      type output_t = string * int * int * int64 array * int64 array * string * string
      let string_of_input_t = Test_printers.(assoc_list string string)
      let string_of_output_t = Test_printers.(tuple7 string int int (array int64) (array int64) string string)
    end

    let transform = fun record ->
      let open Map_check in
      getf vendor record,
      getf socket_count record,
      getf cpu_count record,
      getf features_pv record,
      getf features_hvm record,
      getf policy_pv record,
      getf policy_hvm record

    let tests = `QuickAndAutoDocumented [
      ["vendor", "Intel"; "socket_count", "1"; "cpu_count", "1";
       "features_pv", "00000001-00000002-00000003";
       "features_hvm", "0000000a-0000000b-0000000c";
       "policy_pv", "";
       "polivy_hvm", ""],
      ("Intel", 1, 1, [| 1L; 2L; 3L |], [| 0xaL; 0xbL; 0xcL |], "", "");
      ["vendor", "Amd"; "socket_count", "6"; "cpu_count", "24";
       "features_pv", "00000001";
       "features_hvm", "";
       "policy_pv", "";
       "polivy_hvm", ""],
      ("Amd", 6, 24, [| 1L |], [| |], "", "");
    ]
  end)

module Setters = Generic.MakeStateless (struct
    module Io = struct
      type input_t = string * int * int * int64 array * int64 array * string * string
      type output_t = (string * string) list
      let string_of_input_t = Test_printers.(tuple7 string int int (array int64) (array int64) string string)
      let string_of_output_t = Test_printers.(assoc_list string string)
    end

    let transform = fun (name, sockets, cpus, f_pv, f_hvm, p_pv, p_hvm)  ->
      let open Map_check in
      []
      |> setf vendor name
      |> setf socket_count sockets
      |> setf cpu_count cpus
      |> setf features_pv f_pv
      |> setf features_hvm f_hvm
      |> setf policy_pv p_pv
      |> setf policy_hvm p_hvm
      |> List.sort compare

    let tests = `QuickAndAutoDocumented [
      ("Intel", 1, 1, [| 1L; 2L; 3L |], [| 0xaL; 0xbL; 0xcL |], "", ""),
      List.sort compare ["vendor", "Intel";
                         "socket_count", "1"; "cpu_count", "1";
                         "features_pv", "00000001-00000002-00000003";
                         "features_hvm", "0000000a-0000000b-0000000c";
                         "policy_pv", "";
                         "policy_hvm", ""];

      ("Amd", 6, 24, [| 1L |], [| |], "", ""),
      List.sort compare ["vendor", "Amd";
                         "socket_count", "6"; "cpu_count", "24";
                         "features_pv", "00000001";
                         "features_hvm", "";
                         "policy_pv", "";
                         "policy_hvm", ""]
    ]
  end)

    let string_of_input_t =
      Test_printers.(tuple5 string int int (array int64) (array int64))

    let string_of_output_t = Test_printers.(assoc_list string string)
  end

  let transform (name, sockets, cpus, pv, hvm) =
    let open Map_check in
    []
    |> setf vendor name
    |> setf socket_count sockets
    |> setf cpu_count cpus
    |> setf features_pv pv
    |> setf features_hvm hvm
    |> List.sort compare

  let tests =
    `QuickAndAutoDocumented
      [
        ( ("Intel", 1, 1, [|1L; 2L; 3L|], [|0xaL; 0xbL; 0xcL|])
        , List.sort compare
            [
              ("vendor", "Intel")
            ; ("socket_count", "1")
            ; ("cpu_count", "1")
            ; ("features_pv", "00000001-00000002-00000003")
            ; ("features_hvm", "0000000a-0000000b-0000000c")
            ] )
      ; ( ("Amd", 6, 24, [|1L|], [||])
        , List.sort compare
            [
              ("vendor", "Amd")
            ; ("socket_count", "6")
            ; ("cpu_count", "24")
            ; ("features_pv", "00000001")
            ; ("features_hvm", "")
            ] )
      ]
end)

module Modifiers = Generic.MakeStateless (struct
    module Io = struct
      type input_t = (string * string) list
      type output_t = (string * string) list
      let string_of_input_t = Test_printers.(assoc_list string string)
      let string_of_output_t = Test_printers.(assoc_list string string)
    end

    let transform = fun record ->
      let open Map_check in
      record
      |> setf vendor (getf vendor record)
      |> setf socket_count (getf socket_count record)
      |> setf cpu_count (getf cpu_count record)
      |> setf features_pv (getf features_pv record)
      |> setf features_hvm (getf features_hvm record)
      |> setf policy_pv (getf policy_pv record)
      |> setf policy_hvm (getf policy_hvm record)
      |> List.sort compare

    let tests = `QuickAndAutoDocumented [
      ["cpu_count", "1";
       "features_hvm", "0000000a-0000000b-0000000c";
       "features_pv", "00000001-00000002-00000003";
       "policy_pv", "";
       "policy_hvm", "";
       "socket_count", "1";
       "vendor", "Intel"],
      ["cpu_count", "1";
       "features_hvm", "0000000a-0000000b-0000000c";
       "features_pv", "00000001-00000002-00000003";
       "policy_pv", "";
       "policy_hvm", "";
       "socket_count", "1";
       "vendor", "Intel"];
    ]
  end)

    let string_of_input_t = Test_printers.(assoc_list string string)

    let string_of_output_t = Test_printers.(assoc_list string string)
  end

  let transform record =
    let open Map_check in
    record
    |> setf vendor (getf vendor record)
    |> setf socket_count (getf socket_count record)
    |> setf cpu_count (getf cpu_count record)
    |> setf features_pv (getf features_pv record)
    |> setf features_hvm (getf features_hvm record)
    |> List.sort compare

  let tests =
    `QuickAndAutoDocumented
      [
        ( [
            ("cpu_count", "1")
          ; ("features_hvm", "0000000a-0000000b-0000000c")
          ; ("features_pv", "00000001-00000002-00000003")
          ; ("socket_count", "1")
          ; ("vendor", "Intel")
          ]
        , [
            ("cpu_count", "1")
          ; ("features_hvm", "0000000a-0000000b-0000000c")
          ; ("features_pv", "00000001-00000002-00000003")
          ; ("socket_count", "1")
          ; ("vendor", "Intel")
          ] )
      ]
end)

let domain_type : API.domain_type Test_printers.printer =
  Record_util.domain_type_to_string

module ResetCPUFlags = Generic.MakeStateful (struct
  module Io = struct
    type input_t = (string * API.domain_type) list

    type output_t = string list

    let string_of_input_t = Test_printers.(list (pair string domain_type))

    let string_of_output_t = Test_printers.(list string)
  end

  module State = Test_state.XapiDb

  let features_hvm = "feedface-feedface"
  let features_pv  = "deadbeef-deadbeef"
  let policy_pv = "some-policy-pv"
  let policy_hvm  = "some-policy-hvm"

  let load_input __context cases =
    let cpu_info = [
      "cpu_count", "1";
      "socket_count", "1";
      "vendor", "Abacus";
      "features_pv", features_pv;
      "features_hvm", features_hvm;
      "policy_pv", policy_pv;
      "policy_hvm", policy_hvm
    ] in
    List.iter (fun self -> Db.Host.set_cpu_info ~__context ~self ~value:cpu_info) (Db.Host.get_all ~__context);
    Db.Pool.set_cpu_info ~__context ~self:(Db.Pool.get_all ~__context |> List.hd) ~value:cpu_info;

    let vms = List.map
        (fun (name_label, domain_type) ->
          Test_common.make_vm ~__context ~name_label ~domain_type ())
        cases
    in
    List.iter (fun vm -> Cpuid_helpers.reset_cpu_flags ~__context ~vm) vms

  let extract_output __context vms =
    let get_flags (label, _) =
      let self = List.hd (Db.VM.get_by_name_label ~__context ~label) in
      let flags = Db.VM.get_last_boot_CPU_flags ~__context ~self in
      try List.assoc Xapi_globs.cpu_info_policy_key flags
      with Not_found -> "Policy Not found"
    in List.map get_flags vms


  (* Tuples of ((policy_hvm * policy_pv) list, (expected last_boot_CPU_flags) *)
  let tests = `QuickAndAutoDocumented [
    (["a", `hvm], [policy_hvm]);
    (["a", `pv], [policy_pv]);
    (["a", `pv_in_pvh], [policy_hvm]);
    (["a", `hvm; "b", `pv; "c", `pv_in_pvh],
      [policy_hvm; policy_pv; policy_hvm]);
  ]
end)

let string_of_unit_result =
  Fmt.(str "%a" Dump.(result ~ok:(any "()") ~error:exn))

let tests =
  make_suite "cpuid_helpers_"
  [
    "string_of_features", StringOfFeatures.tests;
    "features_of_string", FeaturesOfString.tests;
    "roundtrip_features_to_features", RoundTripFeaturesToFeatures.tests;
    "roundtrip_string_to_features", RoundTripStringToString.tests;
    "parse_failure", ParseFailure.tests;
    "extend", Extend.tests;
    "zero_extend", ZeroExtend.tests;
    "accessors", Accessors.tests;
    "setters", Setters.tests;
    "modifiers", Modifiers.tests;
    "reset_cpu_flags", ResetCPUFlags.tests;
  ]
