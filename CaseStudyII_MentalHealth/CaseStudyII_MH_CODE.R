library(DataExplorer)

#Read in File
MHData <- read.csv("mental-heath-in-tech-2016_20161114.csv", header=TRUE, sep=",")

#Update Column Names
colnames(MHData)[colnames(MHData)=='Are.you.self.employed.'] <- 'SelfEmployed'
colnames(MHData)[colnames(MHData)=='How.many.employees.does.your.company.or.organization.have.'] <- 'CompanySize'
colnames(MHData)[colnames(MHData)=='Is.your.employer.primarily.a.tech.company.organization.'] <- 'TechCompany'
colnames(MHData)[colnames(MHData)=='Is.your.primary.role.within.your.company.related.to.tech.IT.'] <- 'RoleIT'
colnames(MHData)[colnames(MHData)=='Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.'] <- 'ProvideMHCoverage'
colnames(MHData)[colnames(MHData)=='Do.you.know.the.options.for.mental.health.care.available.under.your.employer.provided.coverage.'] <- 'AwareOfCoverage'
colnames(MHData)[colnames(MHData)=='Has.your.employer.ever.formally.discussed.mental.health..for.example..as.part.of.a.wellness.campaign.or.other.official.communication..'] <- 'CompanyDiscussMH'
colnames(MHData)[colnames(MHData)=='Does.your.employer.offer.resources.to.learn.more.about.mental.health.concerns.and.options.for.seeking.help.'] <- 'CompanyOfferResources'
colnames(MHData)[colnames(MHData)=='Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.provided.by.your.employer.'] <- 'AnonymityProtected'
colnames(MHData)[colnames(MHData)=='If.a.mental.health.issue.prompted.you.to.request.a.medical.leave.from.work..asking.for.that.leave.would.be.'] <- 'MHPromptedMedLeave'
colnames(MHData)[colnames(MHData)=='Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences.'] <- 'DiscussMHCompanyNegative'
colnames(MHData)[colnames(MHData)=='Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences.'] <- 'DiscussPHCompanyNegative'
colnames(MHData)[colnames(MHData)=='Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.'] <- 'DiscussMHWithCoWorkers'
colnames(MHData)[colnames(MHData)=='Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..'] <- 'DiscussMHWithBoss'
colnames(MHData)[colnames(MHData)=='Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.'] <- 'CompanySeriousMHasPH'
colnames(MHData)[colnames(MHData)=='Have.you.heard.of.or.observed.negative.consequences.for.co.workers.who.have.been.open.about.mental.health.issues.in.your.workplace.'] <- 'ObsNegOpenWithMH'
colnames(MHData)[colnames(MHData)=='Do.you.have.medical.coverage..private.insurance.or.state.provided..which.includes.treatment.of.�.mental.health.issues.'] <- 'MedicalCoverage'
colnames(MHData)[colnames(MHData)=='Do.you.know.local.or.online.resources.to.seek.help.for.a.mental.health.disorder.'] <- 'KnowMHResources'
colnames(MHData)[colnames(MHData)=='If.you.have.been.diagnosed.or.treated.for.a.mental.health.disorder..do.you.ever.reveal.this.to.clients.or.business.contacts.'] <- 'MHDisclosedToClients'
colnames(MHData)[colnames(MHData)=='If.you.have.revealed.a.mental.health.issue.to.a.client.or.business.contact..do.you.believe.this.has.impacted.you.negatively.'] <- 'MHDisclosedToClientsWithNegActions'
colnames(MHData)[colnames(MHData)=='If.you.have.been.diagnosed.or.treated.for.a.mental.health.disorder..do.you.ever.reveal.this.to.coworkers.or.employees.'] <- 'MHDisclosedToCoWorkers'
colnames(MHData)[colnames(MHData)=='If.you.have.revealed.a.mental.health.issue.to.a.coworker.or.employee..do.you.believe.this.has.impacted.you.negatively.'] <- 'MHDisclosedToCoWorkersWithNegActions'
colnames(MHData)[colnames(MHData)=='Do.you.believe.your.productivity.is.ever.affected.by.a.mental.health.issue.'] <- 'MHAffectProductivity'
colnames(MHData)[colnames(MHData)=='If.yes..what.percentage.of.your.work.time..time.performing.primary.or.secondary.job.functions..is.affected.by.a.mental.health.issue.'] <- 'PercWorkAffectedByMH'
colnames(MHData)[colnames(MHData)=='Do.you.have.previous.employers.'] <- 'PrevCo'
colnames(MHData)[colnames(MHData)=='Have.your.previous.employers.provided.mental.health.benefits.'] <- 'PrevCoProvideMH'
colnames(MHData)[colnames(MHData)=='Were.you.aware.of.the.options.for.mental.health.care.provided.by.your.previous.employers.'] <- 'PrevCoAwareMHCoverage'
colnames(MHData)[colnames(MHData)=='Did.your.previous.employers.ever.formally.discuss.mental.health..as.part.of.a.wellness.campaign.or.other.official.communication..'] <- 'PrevCoDiscussMH'
colnames(MHData)[colnames(MHData)=='Did.your.previous.employers.provide.resources.to.learn.more.about.mental.health.issues.and.how.to.seek.help.'] <- 'PrevCoOfferResources'
colnames(MHData)[colnames(MHData)=='Was.your.anonymity.protected.if.you.chose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.with.previous.employers.'] <- 'PrevCoAnonymityProtected'
colnames(MHData)[colnames(MHData)=='Do.you.think.that.discussing.a.mental.health.disorder.with.previous.employers.would.have.negative.consequences.'] <- 'PrevCoDiscussMHCompanyNegative'
colnames(MHData)[colnames(MHData)=='Do.you.think.that.discussing.a.physical.health.issue.with.previous.employers.would.have.negative.consequences.'] <- 'PrevCoDiscussPHCompanyNegative'
colnames(MHData)[colnames(MHData)=='Would.you.have.been.willing.to.discuss.a.mental.health.issue.with.your.previous.co.workers.'] <- 'PrevCoDiscussMHWithCoWorkers'
colnames(MHData)[colnames(MHData)=='Would.you.have.been.willing.to.discuss.a.mental.health.issue.with.your.direct.supervisor.s..'] <- 'PrevCoDiscussMHWithBoss'
colnames(MHData)[colnames(MHData)=='Did.you.feel.that.your.previous.employers.took.mental.health.as.seriously.as.physical.health.'] <- 'PrevCoCompanySeriousMHasPH'
colnames(MHData)[colnames(MHData)=='Did.you.hear.of.or.observe.negative.consequences.for.co.workers.with.mental.health.issues.in.your.previous.workplaces.'] <- 'PrevCoObsNegOpenWithMH'
colnames(MHData)[colnames(MHData)=='Would.you.be.willing.to.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview.'] <- 'PotDiscussPH'
colnames(MHData)[colnames(MHData)=='Why.or.why.not.'] <- 'PotDiscussPH_Why'
colnames(MHData)[colnames(MHData)=='Would.you.bring.up.a.mental.health.issue.with.a.potential.employer.in.an.interview.'] <- 'PotDiscussMH'
colnames(MHData)[colnames(MHData)=='Why.or.why.not..1'] <- 'PotDiscussMH_Why'
colnames(MHData)[colnames(MHData)=='Do.you.feel.that.being.identified.as.a.person.with.a.mental.health.issue.would.hurt.your.career.'] <- 'MHHurtCareer'
colnames(MHData)[colnames(MHData)=='Do.you.think.that.team.members.co.workers.would.view.you.more.negatively.if.they.knew.you.suffered.from.a.mental.health.issue.'] <- 'CoWorkersViewYouNegKnewMH'
colnames(MHData)[colnames(MHData)=='How.willing.would.you.be.to.share.with.friends.and.family.that.you.have.a.mental.illness.'] <- 'DiscloseMHFamilyFriends'
colnames(MHData)[colnames(MHData)=='Have.you.observed.or.experienced.an.unsupportive.or.badly.handled.response.to.a.mental.health.issue.in.your.current.or.previous.workplace.'] <- 'NegResponseWithMH'
colnames(MHData)[colnames(MHData)=='Have.your.observations.of.how.another.individual.who.discussed.a.mental.health.disorder.made.you.less.likely.to.reveal.a.mental.health.issue.yourself.in.your.current.workplace.'] <- 'LessLikelyDiscloseMHPreviousEncounter'
colnames(MHData)[colnames(MHData)=='Do.you.have.a.family.history.of.mental.illness.'] <- 'FamilyHistoryMH'
colnames(MHData)[colnames(MHData)=='Have.you.had.a.mental.health.disorder.in.the.past.'] <- 'MHDisorderPast'
colnames(MHData)[colnames(MHData)=='Do.you.currently.have.a.mental.health.disorder.'] <- 'MHCurrently'
colnames(MHData)[colnames(MHData)=='If.yes..what.condition.s..have.you.been.diagnosed.with.'] <- 'MHCurrentlyDiagnosed'
colnames(MHData)[colnames(MHData)=='If.maybe..what.condition.s..do.you.believe.you.have.'] <- 'MHCurrentlyDiagnosedConditions'
colnames(MHData)[colnames(MHData)=='Have.you.been.diagnosed.with.a.mental.health.condition.by.a.medical.professional.'] <- 'MHDiagnosedByDoc'
colnames(MHData)[colnames(MHData)=='If.so..what.condition.s..were.you.diagnosed.with.'] <- 'MHDiagnosedByDoc_Diagnosis'
colnames(MHData)[colnames(MHData)=='Have.you.ever.sought.treatment.for.a.mental.health.issue.from.a.mental.health.professional.'] <- 'MHSoughtTreatment'
colnames(MHData)[colnames(MHData)=='If.you.have.a.mental.health.issue..do.you.feel.that.it.interferes.with.your.work.when.being.treated.effectively.'] <- 'MHInteferesWhenTreated'
colnames(MHData)[colnames(MHData)=='If.you.have.a.mental.health.issue..do.you.feel.that.it.interferes.with.your.work.when.NOT.being.treated.effectively.'] <- 'MHInteferesWhenNotTreated'
colnames(MHData)[colnames(MHData)=='What.is.your.age.'] <- 'Age'
colnames(MHData)[colnames(MHData)=='What.is.your.gender.'] <- 'Gender'
colnames(MHData)[colnames(MHData)=='What.country.do.you.live.in.'] <- 'Country'
colnames(MHData)[colnames(MHData)=='What.US.state.or.territory.do.you.live.in.'] <- 'State'
colnames(MHData)[colnames(MHData)=='What.country.do.you.work.in.'] <- 'CountryWorkIn'
colnames(MHData)[colnames(MHData)=='What.US.state.or.territory.do.you.work.in.'] <- 'StateWorkIn'
colnames(MHData)[colnames(MHData)=='Which.of.the.following.best.describes.your.work.position.'] <- 'CurrentPosition'
colnames(MHData)[colnames(MHData)=='Do.you.work.remotely.'] <- 'WorkRemotely'

#List Column names
colnames(MHData)

#Graphical representation of missing vaules using 'DataExporer' library
plot_missing(MHData, title = "Percent of Missing Values")

#Function to count all NA's in columns
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      na_count=sum(is.na(x)),
      Obs=length(x), 
      perc_missing=sum(is.na(x))/length(x)*100
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$na_count, decreasing=TRUE), ])
}

#show results of NA's counted
propmiss(MHData)

