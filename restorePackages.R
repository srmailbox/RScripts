write.csv(installed.packages(), file="installed.packages.csv")

instll = read.csv("installed.packages.csv")[,"Package"]

for (pkg in instll)
  if(!pkg %in% installed.packages()[,"Package"])
    install.packages(pkg)