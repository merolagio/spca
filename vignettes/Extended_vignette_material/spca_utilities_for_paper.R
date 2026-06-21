
R2latex = function(x, caption, label, position = c("H", "h"), width = "0.95\\linewidth") {
  txt = capture.output(x)
  sub("NA", "", x)
  cat(paste0("\\begin{table}[",
             ifelse(position[1] == "h","!ht]\n", "H]\n")))
  cat("\\centering\n")
  cat("\\begin{minipage}{", width, "}\n", sep = "")
  cat("\\begin{verbatim}\n")
  cat(txt, sep = "\n")
  cat("\n\\end{verbatim}\n")
  cat("\\end{minipage}\n")
  cat("\\caption{", caption, "}\\label{", label, "}\n", sep = "")
  cat("\\end{table}\n")
}
