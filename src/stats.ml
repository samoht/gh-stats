open Github.Monad
open Astring

let src = Logs.Src.create "gh-stats" ~doc:"Compute statistics from GitHub"
module Log = (val Logs.src_log src : Logs.LOG)

let contr_of_repo ~token (user, repo) =
  let users = Github.Repo.contributors ~token ~user ~repo () in
  Github.Stream.to_list users >|= fun users ->
  let users = List.map (fun u ->
      u.Github_t.contributor_login,
      u.Github_t.contributor_contributions
    ) users in
  String.Map.of_list users

let is_mirror r =
  match r.Github_t.repository_description with
  | None   -> false
  | Some d ->
  try let _ = Re.(exec (compile @@ no_case (str "mirror"))) d in true
  with Not_found -> false

let is_fork r = r.Github_t.repository_fork

let merge_map = String.Map.union (fun _ x y -> Some (x+y))

let contr_of_user ~token user =
  let repos = Github.User.repositories ~token ~user () in
  Github.Stream.to_list repos >>= fun repos ->
  let rec aux acc = function
    | []   -> return acc
    | h::t ->
      let repo = h.Github_t.repository_name in
      if is_fork h then (
        Log.info (fun l -> l "Skipping %s: it's a fork!" repo);
        aux acc t
      ) else if is_mirror h then (
        Log.info (fun l -> l "Skipping %s: it's a mirror!" repo);
        aux acc t
      ) else (
        Log.info (fun l -> l "Processing %s." repo);
        contr_of_repo ~token (user, repo) >>= fun contr ->
        aux (merge_map acc contr) t
      )
  in
  aux String.Map.empty repos

let pp_contr ppf (user, count) = Fmt.pf ppf "%s: %d" user count

let pp_list pp ppf = function
  | []   -> ()
  | h::t ->
    pp ppf h;
    let rec aux = function
      | []   -> ()
      | [h]  -> Fmt.pf ppf " and %a" pp h
      | h::t -> Fmt.pf ppf ", %a" pp h; aux t
  in
  aux t

let pp_contr users ppf map =
  let len   = String.Map.cardinal map in
  let total = String.Map.fold (fun _ -> (+)) map 0 in
  let list  = String.Map.bindings map in
  let list  = List.sort (fun (_, x) (_, y) -> compare y x) list in
  let name ppf = pp_list Fmt.string ppf users in
  let has ppf = match users with
    | [_] -> Fmt.string ppf "has"
    | _   -> Fmt.string ppf "have"
  in
  let pp_count = Fmt.(styled `Bold int) in
  Fmt.pf ppf "%t %t %a contributors (for a total of %a contributions).\n%!"
    name has pp_count len pp_count total;
  List.iter (fun x -> Fmt.pf ppf "%a\n" pp_contr x) list

let run fn = Lwt_main.run (Github.Monad.run (fn ()))

let main token users =
  run @@ fun () ->
  let rec aux acc = function
    | []   -> return acc
    | h::t ->
      contr_of_user ~token h >>= fun contr ->
      aux (merge_map acc contr) t
  in
  aux String.Map.empty users >|= fun contr ->
  Fmt.pr "%a" (pp_contr users) contr

(* CLI *)

open Cmdliner

let footer = [
  `S "AUTHORS";
  `P "Thomas Gazagnaire <thomas@gazagnaire.org>";
  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/gh-stats/issues.";
]

let setup_log =
  let f style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ());
  in
  Term.(const f $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let token =
  let env = Arg.env_var "GH_TOKEN" in
  let doc = Arg.info ["t";"token"]
      ~env ~docv:"TOKEN" ~doc:"A valid GitHub token."
  in
  let t = Arg.(required & opt (some string) None doc) in
  Term.(pure Github.Token.of_string $ t)

let users =
  let doc = Arg.info []
      ~docv:"[USER|ORG]" ~doc:"A list of GitHub users or organisations"
  in
  Arg.(non_empty & pos_all string [] doc)

let term =
  let doc = "A tool to aggregate statistics for GitHub organisations." in
  Term.(pure (fun () -> main) $ setup_log $ token $ users),
  Term.info "gh-stats" ~version:"%%VERSION%%" ~doc ~man:footer

let () = match Term.eval term with `Error _ -> exit 1 | _ -> ()
