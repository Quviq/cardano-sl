-module(tix2eqc).
-compile(export_all).

run() ->
  compile:file(tix2eqc_data),
  code:load_file(tix2eqc_data),
  code:unstick_mod(erl_scan),
  {module, _} = code:load_abs("erl_scan/haskell_scan"),
  {module, _} = code:load_abs("erl_scan/erl_scan"),
  file:set_cwd(all),
  eqc_cover:write_html(tix2eqc_data:data(), []),
  erlang:halt().
