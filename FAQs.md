
## The design of Zelm

+ Why have a separate `custom-package-repository-config.json` file in
  `$ELM_HOME` instead of just extending `elm.json` even more? Zelm aims to be
  compatible with current Elm tooling, especially IDE tooling. Current IDE
  integrations assume that there is a global cache of packages located in
  `$ELM_HOME` rather than local per-project caches (as is e.g. the case with
  `npm`). This means that to play nicely with these tooling choices, Zelm also
  reuses the same global cache. However, because the cache is global to
  `$ELM_HOME`, while many Elm projects can use the same `$ELM_HOME`, if one project has a
  `custom-package-repository-config.json` that declares one location for a
  given package while another project declares another location and those two
  locations do not have the same code for that package, one project can break
  another. This sort of breakage can appear to be non-deterministic to a user
  and can be extremely frustrating to debug. To avoid this, we colocate
  `custom-package-repository-config.json` with $ELM_HOME.
+ More FAQs forthcoming...
