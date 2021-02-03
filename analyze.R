library(data.table)
csv.vec <- Sys.glob("2*/*.csv")
data.dt.list <- list()
for(data.csv in csv.vec){
  dt <- if(grepl("/Transactions", data.csv)){
    data.table::fread(data.csv, blank.lines.skip=TRUE, header=TRUE)[, .(
      desc=`Merchant Name`,
      dollars=-as.numeric(sub("[(]", "-", gsub("[$)]", "", Amount)))
    )]
  }else if(grepl("_download.csv", data.csv)){
    data.table::fread(data.csv)[, .(
      desc=Description,
      dollars=ifelse(is.na(Debit), Credit, -Debit)
    )]
  }else if(basename(data.csv)=="export.csv"){
    data.table::fread(data.csv, fill=TRUE)[, .(
      desc=Description,
      dollars=as.numeric(gsub("[$,]", "", ifelse(Credit=="", Debit, Credit)))
    )]
  }else{
    stop(data.csv, " not handled")
  }
  month <- dirname(data.csv)
  data.dt.list[[data.csv]] <- data.table(month, dt)
}
data.dt <- do.call(rbind, data.dt.list)
data.dt[, category := {
  g <- function(s)grepl(s, desc)
  data.table::fcase(
    g("AMAZON"), "food",
    g("CITY OF FLAG"), "city",
    g("SAMS ?CLUB"), "food",
    g("WHOLEFDS"), "food",
    g("SUDDENLINK"), "internet",
    g("UNS GAS"), "gas",
    g("GEICO"), "car",
    g("EDWARD JONES"), "savings",
    g("SPROUTS"), "food",
    g("ARIZONA PUBL SVC"), "elec",
    g("VZWRLSS"), "telephone",
    g("CELEBRAN"), "formation",
    g("Audible|DisneyPLUS|APPLE.COM/BILL"), "entertainment",
    g("THRIFTBOOKS"), "formation",
    g("TARGET"), "food",
    g("Wal-Mart"), "food",
    g("POSHMARK"), "baz",
    g("SKI"), "sport",
    g("FRYS"), "food",
    g("BICYCLE"), "sport",
    g("ATHLETIC CLUB"), "sport",
    g("NATURAL GROCERS"), "food",
    g("SHARE DRAFT"), "rent",
    g("RED ROCKS PARKING"), "sport",
    g("FLAGSTAFF COOPERATIVE"), "garderie",
    g("NAU FOUNDATION"), "donation",
    g("CR CRD TXN"), "delete",
    g("CAPITAL ONE"), "delete",
    g("PAYMENT - THANK YOU"), "delete",
    0 < dollars, "income",
    default="other")
}]  
data.dt[category=="other", desc]
data.dt[category=="income"]

total.dt <- data.dt[, .(
  total=sum(dollars)
), by=category][order(total)]
expected <- c(
  college=160,
  garderie=500,
  rent=1600,
  car=200,
  gas=100,
  internet=50,
  city=100,
  elec=50,
  food=800,
  donation=5,
  formation=100,
  self=50,
  telephone=66,
  economie=200,
  sortie=100,
  vacation=500,
  baz=50,
  sport=100,
  sax=20)
all.dt <- total.dt[union(category, names(expected)), on="category"]
all.dt[, expected := -expected[category] ]
all.dt[is.na(total), total := 0]
all.dt[, diff := total - expected]
all.dt[order(diff)]
