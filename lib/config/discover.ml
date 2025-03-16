module C = Configurator.V1

let rec link ?(flag = "-l") = function
  | [] -> []
  | lib :: libs -> "-cclib" :: (flag ^ " " ^ lib) :: link ~flag libs

let () =
  C.main ~name:"suplex" (fun c ->
      let library_flags =
        match C.ocaml_config_var c "system" with
        | Some ("linux" | "linux_elf" | "elf") -> link ["externals"; "stdc++"]
        | Some "macosx" -> link ["externals"; "stdc++"]
        | Some "mingw64" ->
            (* TODO *)
            link ["externals"; "stdc++"]
        | Some ("netbsd" | "freebsd" | "openbsd" | "bsd" | "bsd_elf") ->
            link ["externals"; "stdc++"]
        | Some system -> C.die "unsupported system: %s" system
        | None -> C.die "unsupported system"
      in
      C.Flags.write_sexp "library_flags.sexp" library_flags)
