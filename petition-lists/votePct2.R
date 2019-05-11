
# To run: big.data$Vote.Percent.2 <- calculateVotePct(big.data)
calculateVotePct<- function(data) {
	apply(data, 1, votePct2)
}

votePct2<- function(voter) {
	pctVoting(voteRecord(voter))
}

voteRecord<- function(voter) {
	regDate<-toDate(regDate(voter))
	muniPri<- electionField(muniPrimariesSince(regDate))
	genPri<- electionField(generalPrimariesSince(regDate))
	muniEl<- electionField(muniElectionsSince(regDate))
	genEl<- electionField(generalElectionsSince(regDate))
	elections<- c(muniPri,muniEl,genPri,genEl)
	record<- electionRecord(elections, voter)
	return(record)
}

pctVoting<- function(record) {
	ifelse(length(record)==0, 0.0, sum(record)/length(record))
}

regDate<- function(voter) {
	voter["Registration.Date"]
}

toDate<- function(date) {
	tryCatch({ 
	year<- strsplit(date, "/")[[1]][[3]]
	form<- ifelse(year < "20", "20%y-%m-%d", "19%y-%m-%d") 
	return(format(as.Date(date, "%m/%d/%y"), form))
	}, error=function(e) { "2019-05-21" })
}

electionRecord<- function(elections, voter) {
	return(sapply(elections, function(col) { voted(voter[col]) }))
}

electionField<- function(num) {
	paste("Election.", num, ".Vote.Method", sep="")
}

afterDate<- function(vals, v) {
	Filter(function(x) { v < x }, vals)
}

voted<- function(val) { return(val!="" & ! is.na(val)) }

fromEnd<- function(vec, i) {
	if (i >= length(vec)) {
		return(vec)
	} else {
		return(vec[-(0:(length(vec) - i))])
	}
}

# "PHILADELPHIA"	1	"2004 GENERAL ELECTION"	"11/02/2004"
# "PHILADELPHIA"	15	"2006 GENERAL ELECTION"	"11/07/2006"
# "PHILADELPHIA"	17	"2007 GENERAL ELECTION"	"11/06/2007"
# "PHILADELPHIA"	19	"2008 GENERAL ELECTION"	"11/04/2008"
# "PHILADELPHIA"	23	"2010 GENERAL ELECTION"	"11/02/2010"
# "PHILADELPHIA"	28	"2012 GENERAL ELECTION"	"11/06/2012"
# "PHILADELPHIA"	32	"2014 GENERAL ELECTION"	"11/04/2014"
# "PHILADELPHIA"	39	"2016 GENERAL ELECTION"	"11/08/2016"
# "PHILADELPHIA"	6	"2018 GENERAL ELECTION"	"11/06/2018"
generalElectionsSince<- function(date) {
	elections<- c(1,15,17,19,23,28,32,39,6)
	dates<- c("2004-11-02","2006-11-07","2007-11-06","2008-11-04","2010-11-02","2012-11-06","2014-11-04","2016-11-08","2018-11-07")
	return(fromEnd(elections, length(afterDate(dates, date))))
}

# "PHILADELPHIA"	10	"2004 GENERAL PRIMARY"	"04/27/2004"
# "PHILADELPHIA"	14	"2006 GENERAL PRIMARY"	"05/16/2006"
# "PHILADELPHIA"	16	"2007 GENERAL PRIMARY"	"05/15/2007"
# "PHILADELPHIA"	18	"2008 GENERAL PRIMARY"	"04/22/2008"
# "PHILADELPHIA"	22	"2010 GENERAL PRIMARY"	"05/18/2010"
# "PHILADELPHIA"	27	"2012 GENERAL PRIMARY"	"04/24/2012"
# "PHILADELPHIA"	31	"2014 GENERAL PRIMARY"	"05/20/2014"
# "PHILADELPHIA"	38	"2016 GENERAL PRIMARY"	"04/26/2016"
# "PHILADELPHIA"	7	"2018 GENERAL PRIMARY"	"05/15/2018"
generalPrimariesSince<- function(date) {
	elections<- c(10,14,16,18,22,27,31,38,7)
	dates<- c("2004-04-27","2006-05-16","2007-05-15","2008-04-22","2010-05-18","2012-04-24","2014-05-20","2016-04-26","2018-05-15")
	return(fromEnd(elections, length(afterDate(dates, date))))
}

# "PHILADELPHIA"	8	"2003 MUNICIPAL ELECTION"	"11/04/2003"
# "PHILADELPHIA"	2	"2005 MUNICIPAL ELECTION"	"11/08/2005"
# "PHILADELPHIA"	21	"2009 MUNICIPAL GENERAL ELECTION"	"11/03/2009"
# "PHILADELPHIA"	26	"2011 MUNICIPAL ELECTION"	"11/08/2011"
# "PHILADELPHIA"	30	"2013 MUNICIPAL ELECTION"	"11/05/2013"
# "PHILADELPHIA"	36	"2015 MUNICIPAL ELECTION"	"11/03/2015"
# "PHILADELPHIA"	5	"2017 MUNICIPAL ELECTION"	"11/07/2017"
muniElectionsSince<- function(date) {
	elections<- c(8,2,21,26,30,36,5)
	dates<- c("2003-11-04","2005-11-08","2009-11-03","2011-11-08","2013-11-05","2015-11-03","2017-11-07")
	return(fromEnd(elections, length(afterDate(dates, date))))
}

# "PHILADELPHIA"	9	"2003 MUNICIPAL PRIMARY"	"05/20/2003"
# "PHILADELPHIA"	3	"2005 MUNICIPAL PRIMARY"	"05/17/2005"
# "PHILADELPHIA"	20	"2009 MUNICIPAL PRIMARY ELECTION"	"05/19/2009"
# "PHILADELPHIA"	25	"2011 MUNICIPAL PRIMARY"	"05/17/2011"
# "PHILADELPHIA"	29	"2013 MUNICIPAL PRIMARY"	"05/21/2013"
# "PHILADELPHIA"	34	"2015 MUNICIPAL PRIMARY"	"05/19/2015"
# "PHILADELPHIA"	4	"2017 MUNICIPAL PRIMARY"	"05/16/2017"
muniPrimariesSince<- function(date) {
	elections<- c(9,3,20,25,29,34,4)
	dates<- c("2003-05-20","2005-05-17","2009-05-19","2011-05-17","2013-05-21","2015-05-19","2017-05-16")
	return(fromEnd(elections, length(afterDate(dates, date))))
}

