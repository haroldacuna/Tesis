source("scripts/00_config_did.R")
oficial <- all.vars(COVARIABLES_XFORMLA)
for (f in c("scripts/46_event_study_tres_estimadores.R", "scripts/47_linea_base_bootstrap.R")) {
  txt <- paste(readLines(f, warn = FALSE), collapse = " ")
  m <- regmatches(txt, regexpr("COVS\\s*<-\\s*c\\([^)]*\\)", txt))
  v <- eval(parse(text = sub("COVS\\s*<-\\s*", "", m)))
  if (setequal(v, oficial)) {
    cat(basename(f), ": IGUAL a la lista oficial\n")
  } else {
    cat(basename(f), ": DIFIERE\n",
        "   le faltan:", paste(setdiff(oficial, v), collapse = ", "), "\n",
        "   le sobran:", paste(setdiff(v, oficial), collapse = ", "), "\n")
  }
}