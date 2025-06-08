library(stm)
library(quanteda)
library(data.table)
set.seed(42)

# Reading and processing the data
data <- read.csv("Embedded_lies_manprep.csv", sep = ";")

## Remove CONSENT_REVOKED, DATA_EXPIRED, and Prefer Not To Say
data <- data[!(data$Sex %in% c("CONSENT_REVOKED", "DATA_EXPIRED", "Prefer not to say")),]

data_noticket <- data[data$Event == "Taking the bus/train without the ticket",]
data_missdeadline <- data[data$Event == "Missing a deadline at work because of bad organisation",]
data_caraccidentinvolve <- data[data$Event == "Being involved in a car accident",]
data_cheatexam <- data[data$Event == "Cheating on an exam",]
data_getfired <- data[data$Event == "Getting fired",]

## Processing text data for each data frame
processed_noticket <- textProcessor(data_noticket$False_event_clean, metadata = data_noticket, stem = FALSE)
processed_missdeadline <- textProcessor(data_missdeadline$False_event_clean, metadata = data_missdeadline, stem = FALSE)
processed_caraccidentinvolve <- textProcessor(data_caraccidentinvolve$False_event_clean, metadata = data_caraccidentinvolve, stem = FALSE)
processed_cheatexam <- textProcessor(data_cheatexam$False_event_clean, metadata = data_cheatexam, stem = FALSE)
processed_getfired <- textProcessor(data_getfired$False_event_clean, metadata = data_getfired, stem = FALSE)


## Checking word removal thresholds
plotRemoved(processed_noticket$documents, lower.thresh = seq(1, 123, by = 1))
plotRemoved(processed_missdeadline$documents, lower.thresh = seq(1, 96, by = 1))
plotRemoved(processed_caraccidentinvolve$documents, lower.thresh = seq(1, 47, by = 1))
plotRemoved(processed_cheatexam$documents, lower.thresh = seq(1, 48, by = 1))
plotRemoved(processed_getfired$documents, lower.thresh = seq(1, 34, by = 1))

## Preparing documents
out_noticket <- prepDocuments(processed_noticket$documents, processed_noticket$vocab, processed_noticket$meta, lower.thresh = 3)
docs_noticket <- out_noticket$documents
vocab_noticket <- out_noticket$vocab
meta_noticket <- out_noticket$meta

out_missdeadline <- prepDocuments(processed_missdeadline$documents, processed_missdeadline$vocab, processed_missdeadline$meta, lower.thresh = 3)
docs_missdeadline <- out_missdeadline$documents
vocab_missdeadline <- out_missdeadline$vocab
meta_missdeadline <- out_missdeadline$meta

out_caraccidentinvolve <- prepDocuments(processed_caraccidentinvolve$documents, processed_caraccidentinvolve$vocab, processed_caraccidentinvolve$meta, lower.thresh = 2)
docs_caraccidentinvolve <- out_caraccidentinvolve$documents
vocab_caraccidentinvolve <- out_caraccidentinvolve$vocab
meta_caraccidentinvolve <- out_caraccidentinvolve$meta

out_cheatexam <- prepDocuments(processed_cheatexam$documents, processed_cheatexam$vocab, processed_cheatexam$meta, lower.thresh = 2)
docs_cheatexam <- out_cheatexam$documents
vocab_cheatexam <- out_cheatexam$vocab
meta_cheatexam <- out_cheatexam$meta

out_getfired <- prepDocuments(processed_getfired$documents, processed_getfired$vocab, processed_getfired$meta, lower.thresh = 2)
docs_getfired <- out_getfired$documents
vocab_getfired <- out_getfired$vocab
meta_getfired <- out_getfired$meta

## Checking topic count
set.seed(42)
storage_noticketwide <- searchK(docs_noticket, vocab_noticket, K = c(4, 6, 8, 10, 12, 14), prevalence = ~Sex, content = ~Sex, data = meta_noticket)
plot(storage_noticketwide)
set.seed(42)
storage_noticketnarrow <- searchK(docs_noticket, vocab_noticket, K = c(8, 9, 10, 11, 12), prevalence = ~Sex, content = ~Sex, data = meta_noticket)
plot(storage_noticketnarrow)

set.seed(42)
storage_missdeadlinewide <- searchK(docs_missdeadline, vocab_missdeadline, K = c(4, 6, 8, 10, 12, 14), prevalence = ~Sex, content = ~Sex, data = meta_missdeadline)
plot(storage_missdeadlinewide)
set.seed(42)
storage_missdeadlinenarrow <- searchK(docs_missdeadline, vocab_missdeadline, K = c(3, 4, 5, 6, 7, 8, 9), prevalence = ~Sex, content = ~Sex, data = meta_missdeadline)
plot(storage_missdeadlinenarrow)

set.seed(42)
storage_caraccidentinvolvewide <- searchK(docs_caraccidentinvolve, vocab_caraccidentinvolve, K = c(4, 6, 8, 10, 12, 14), prevalence = ~Sex, content = ~Sex, data = meta_caraccidentinvolve)
plot(storage_caraccidentinvolvewide)
set.seed(42)
storage_caraccidentinvolvenarrow <- searchK(docs_caraccidentinvolve, vocab_caraccidentinvolve, K = c(3, 4, 5, 6, 7, 8), prevalence = ~Sex, content = ~Sex, data = meta_caraccidentinvolve)
plot(storage_caraccidentinvolvenarrow)

set.seed(42)
storage_cheatexamwide <- searchK(docs_cheatexam, vocab_cheatexam, K = c(4, 6, 8, 10, 12, 14), prevalence = ~Sex, content = ~Sex, data = meta_cheatexam)
plot(storage_cheatexamwide)
set.seed(42)
storage_cheatexamnarrow <- searchK(docs_cheatexam, vocab_cheatexam, K = c(4, 5, 6, 7, 8, 9, 10), prevalence = ~Sex, content = ~Sex, data = meta_cheatexam)
plot(storage_cheatexamnarrow)

set.seed(42)
storage_getfiredwide <- searchK(docs_getfired, vocab_getfired, K = c(4, 6, 8, 10, 12, 14), prevalence = ~Sex, content = ~Sex, data = meta_getfired)
plot(storage_getfiredwide)
set.seed(42)
storage_getfirednarrow <- searchK(docs_getfired, vocab_getfired, K = c(3, 4, 5, 6, 7, 8, 9, 10), prevalence = ~Sex, content = ~Sex, data = meta_getfired)
plot(storage_getfirednarrow)

# Topic Model Estimations

noticket_model <- stm(documents = docs_noticket, vocab = vocab_noticket, K = 10, prevalence = ~Sex, content = ~Sex, max.em.its = 75, data = meta_noticket, init.type = "Spectral", seed = 42)

missdeadline_model <- stm(documents = docs_missdeadline, vocab = vocab_missdeadline, K = 7, prevalence = ~Sex, content = ~Sex, max.em.its = 75, data = meta_missdeadline, init.type = "Spectral", seed = 42)

caraccidentinvolve_model <- stm(documents = docs_caraccidentinvolve, vocab = vocab_caraccidentinvolve, K = 4, prevalence = ~Sex, content = ~Sex, max.em.its = 75, data = meta_caraccidentinvolve, init.type = "Spectral", seed = 42)

cheatexam_model <- stm(documents = docs_cheatexam, vocab = vocab_cheatexam, K = 6, prevalence = ~Sex, content = ~Sex, max.em.its = 75, data = meta_cheatexam, init.type = "Spectral", seed = 42)

getfired_model <- stm(documents = docs_getfired, vocab = vocab_getfired, K = 6, prevalence = ~Sex, content = ~Sex, max.em.its = 75, data = meta_getfired, init.type = "Spectral", seed = 42)

# Topic Model Examples and Definition

## Event 1: No ticket

sageLabels(noticket_model, n = 7)
labelTopics(noticket_model, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

### Topic 1
notickets_thoughts1 <- findThoughts(noticket_model, texts = meta_noticket$False_event, topics = 1, n = 1)$docs[[1]]
par(mfrow = c(1,2), mar=c(1, 1, 1, 1))
plotQuote(notickets_thoughts1, width = 60, main = "Topic 1")
#print(notickets_thoughts1)

### Topic 2
notickets_thoughts2 <- findThoughts(noticket_model, texts = meta_noticket$False_event, topics = 2, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(1, 1, 1, 1))
plotQuote(notickets_thoughts2, width = 60, main = "Topic 2")
#print(notickets_thoughts2)

### Topic 3
notickets_thoughts3 <- findThoughts(noticket_model, texts = meta_noticket$False_event, topics = 3, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(notickets_thoughts3, width = 60, main = "Topic 3")
print(notickets_thoughts3)

### Topic 4
notickets_thoughts4 <- findThoughts(noticket_model, texts = meta_noticket$False_event, topics = 4, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(notickets_thoughts4, width = 60, main = "Topic 4")
print(notickets_thoughts4)

### Topic 5
notickets_thoughts5 <- findThoughts(noticket_model, texts = meta_noticket$False_event, topics = 5, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(notickets_thoughts5, width = 60, main = "Topic 5")
print(notickets_thoughts5)

### Topic 6
notickets_thoughts6 <- findThoughts(noticket_model, texts = meta_noticket$False_event, topics = 6, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(notickets_thoughts6, width = 60, main = "Topic 6")
print(notickets_thoughts6)

### Topic 7
notickets_thoughts7 <- findThoughts(noticket_model, texts = meta_noticket$False_event, topics = 7, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(notickets_thoughts7, width = 60, main = "Topic 7")
print(notickets_thoughts7)

### Topic 8
notickets_thoughts8 <- findThoughts(noticket_model, texts = meta_noticket$False_event, topics = 8, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(notickets_thoughts8, width = 60, main = "Topic 8")
print(notickets_thoughts8)

### Topic 9
notickets_thoughts9 <- findThoughts(noticket_model, texts = meta_noticket$False_event, topics = 9, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(notickets_thoughts9, width = 60, main = "Topic 9")
print(notickets_thoughts9)

### Topic 10
notickets_thoughts10 <- findThoughts(noticket_model, texts = meta_noticket$False_event, topics = 10, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(notickets_thoughts10, width = 60, main = "Topic 10")
print(notickets_thoughts10)

## Event 2: Missed Deadline

sageLabels(missdeadline_model, n = 7)
labelTopics(missdeadline_model, c(1, 2, 3, 4, 5, 6, 7))

### Topic 1
missdeadline_thoughts1 <- findThoughts(missdeadline_model, texts = meta_missdeadline$False_event, topics = 1, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(missdeadline_thoughts1, width = 60, main = "Topic 1")

### Topic 2
missdeadline_thoughts2 <- findThoughts(missdeadline_model, texts = meta_missdeadline$False_event, topics = 2, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(missdeadline_thoughts2, width = 60, main = "Topic 2")

### Topic 3
missdeadline_thoughts3 <- findThoughts(missdeadline_model, texts = meta_missdeadline$False_event, topics = 3, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(missdeadline_thoughts3, width = 60, main = "Topic 3")

### Topic 4
missdeadline_thoughts4 <- findThoughts(missdeadline_model, texts = meta_missdeadline$False_event, topics = 4, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(missdeadline_thoughts4, width = 60, main = "Topic 4")

### Topic 5
missdeadline_thoughts5 <- findThoughts(missdeadline_model, texts = meta_missdeadline$False_event, topics = 5, n = 3)$docs[[1]]
par(mfrow = c(1,1), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(missdeadline_thoughts5, width = 130, main = "Topic 5")

### Topic 6
missdeadline_thoughts6 <- findThoughts(missdeadline_model, texts = meta_missdeadline$False_event, topics = 6, n = 3)$docs[[1]]
par(mfrow = c(1,1), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(missdeadline_thoughts6, width = 130, main = "Topic 6")

### Topic 7
missdeadline_thoughts7 <- findThoughts(missdeadline_model, texts = meta_missdeadline$False_event, topics = 7, n = 5)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(missdeadline_thoughts7, width = 60, main = "Topic 7")

print(missdeadline_thoughts1)
print(missdeadline_thoughts2)
print(missdeadline_thoughts3)
print(missdeadline_thoughts4)
print(missdeadline_thoughts5)
print(missdeadline_thoughts6)
print(missdeadline_thoughts7)

## Event 3: Car Accident Involve

sageLabels(caraccidentinvolve_model, n = 7)
labelTopics(caraccidentinvolve_model, c(1, 2, 3, 4))

### Topic 1
caraccidentinvolve_thoughts1 <- findThoughts(caraccidentinvolve_model, texts = meta_caraccidentinvolve$False_event, topics = 1, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(caraccidentinvolve_thoughts1, width = 60, main = "Topic 1")

### Topic 2
caraccidentinvolve_thoughts2 <- findThoughts(caraccidentinvolve_model, texts = meta_caraccidentinvolve$False_event, topics = 2, n = 5)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(caraccidentinvolve_thoughts2, width = 60, main = "Topic 2")

### Topic 3
caraccidentinvolve_thoughts3 <- findThoughts(caraccidentinvolve_model, texts = meta_caraccidentinvolve$False_event, topics = 3, n = 5)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(caraccidentinvolve_thoughts3, width = 60, main = "Topic 3")

### Topic 4
caraccidentinvolve_thoughts4 <- findThoughts(caraccidentinvolve_model, texts = meta_caraccidentinvolve$False_event, topics = 4, n = 5)$docs[[1]]
par(mfrow = c(1,1), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(caraccidentinvolve_thoughts4, width = 130, main = "Topic 4")

print(caraccidentinvolve_thoughts1)
print(caraccidentinvolve_thoughts2)
print(caraccidentinvolve_thoughts3)
print(caraccidentinvolve_thoughts4)

## Event 4: Cheat exam

sageLabels(cheatexam_model, n = 7)
labelTopics(cheatexam_model, c(1, 2, 3, 4, 5, 6))

### Topic 1
cheatexam_thoughts1 <- findThoughts(cheatexam_model, texts = meta_cheatexam$False_event, topics = 1, n = 4)$docs[[1]]
par(mfrow = c(1,1), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(cheatexam_thoughts1, width = 130, main = "Topic 1")

### Topic 2
cheatexam_thoughts2 <- findThoughts(cheatexam_model, texts = meta_cheatexam$False_event, topics = 2, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(cheatexam_thoughts2, width = 60, main = "Topic 2")

### Topic 3
cheatexam_thoughts3 <- findThoughts(cheatexam_model, texts = meta_cheatexam$False_event, topics = 3, n = 4)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(cheatexam_thoughts3, width = 60, main = "Topic 3")

### Topic 4
cheatexam_thoughts4 <- findThoughts(cheatexam_model, texts = meta_cheatexam$False_event, topics = 4, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(cheatexam_thoughts4, width = 60, main = "Topic 4")

### Topic 5
cheatexam_thoughts5 <- findThoughts(cheatexam_model, texts = meta_cheatexam$False_event, topics = 5, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(cheatexam_thoughts5, width = 60, main = "Topic 5")

### Topic 6
cheatexam_thoughts6 <- findThoughts(cheatexam_model, texts = meta_cheatexam$False_event, topics = 6, n = 5)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(cheatexam_thoughts6, width = 60, main = "Topic 6")

print(cheatexam_thoughts1)
print(cheatexam_thoughts2)
print(cheatexam_thoughts3)
print(cheatexam_thoughts4)
print(cheatexam_thoughts5)
print(cheatexam_thoughts6)

## Event 5: Get fired

sageLabels(getfired_model, n = 7)
labelTopics(getfired_model, c(1, 2, 3, 4, 5, 6))

### Topic 1
getfired_thoughts1 <- findThoughts(getfired_model, texts = meta_getfired$False_event, topics = 1, n = 4)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(getfired_thoughts1, width = 60, main = "Topic 1")

### Topic 2
getfired_thoughts2 <- findThoughts(getfired_model, texts = meta_getfired$False_event, topics = 2, n = 4)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(getfired_thoughts2, width = 60, main = "Topic 2")

### Topic 3
getfired_thoughts3 <- findThoughts(getfired_model, texts = meta_getfired$False_event, topics = 3, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(getfired_thoughts3, width = 60, main = "Topic 3")

### Topic 4
getfired_thoughts4 <- findThoughts(getfired_model, texts = meta_getfired$False_event, topics = 4, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(getfired_thoughts4, width = 60, main = "Topic 4")

### Topic 5
getfired_thoughts5 <- findThoughts(getfired_model, texts = meta_getfired$False_event, topics = 5, n = 5)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(getfired_thoughts5, width = 60, main = "Topic 5")

### Topic 6
getfired_thoughts6 <- findThoughts(getfired_model, texts = meta_getfired$False_event, topics = 6, n = 3)$docs[[1]]
par(mfrow = c(1,2), mar=c(0.5, 0.5, 1, 0.5))
plotQuote(getfired_thoughts6, width = 60, main = "Topic 6")

print(getfired_thoughts1)
print(getfired_thoughts2)
print(getfired_thoughts3)
print(getfired_thoughts4)
print(getfired_thoughts5)
print(getfired_thoughts6)

# Visualizations

noticket_effect <- estimateEffect(1:10 ~ Sex, noticket_model, meta = meta_noticket, uncertainty = "Local", documents = docs_noticket)
plot(noticket_effect, covariate = "Sex", topics = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), model = noticket_model, 
     method = "difference", cov.value1 = "Female", cov.value2 = "Male", xlab = "More Male ... More Female", 
     main = "Event 1: Topic Prevalence", xlim = c(-0.1, 0.1), labeltype = "custom", 
     custom.labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7", "Topic 8", "Topic 9", "Topic 10"))
plot(noticket_model, type = "perspectives", topics = 1)
plot(noticket_model, type = "perspectives", topics = 2)
plot(noticket_model, type = "perspectives", topics = 3)
plot(noticket_model, type = "perspectives", topics = 4)
plot(noticket_model, type = "perspectives", topics = 5)
plot(noticket_model, type = "perspectives", topics = 6)
plot(noticket_model, type = "perspectives", topics = 7)
plot(noticket_model, type = "perspectives", topics = 8)
plot(noticket_model, type = "perspectives", topics = 9)
plot(noticket_model, type = "perspectives", topics = 10)
plot(noticket_model, type = "summary", xlim = c(0, 0.3))
summary(noticket_model)
summary(noticket_effect)

missdeadline_effect <- estimateEffect(1:7 ~ Sex,missdeadline_model, meta = meta_missdeadline, uncertainty = "Local", documents = docs_missdeadline)
plot(missdeadline_effect, covariate = "Sex", topics = c(1, 2, 3, 4, 5, 6, 7), model = missdeadline_model, 
     method = "difference", cov.value1 = "Female", cov.value2 = "Male", xlab = "More Male ... More Female", 
     main = "Event 2: Topic Prevalence", xlim = c(-0.1, 0.1), labeltype = "custom", 
     custom.labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7"))
plot(missdeadline_model, type = "perspectives", topics = 1)
plot(missdeadline_model, type = "perspectives", topics = 2)
plot(missdeadline_model, type = "perspectives", topics = 3)
plot(missdeadline_model, type = "perspectives", topics = 4)
plot(missdeadline_model, type = "perspectives", topics = 5)
plot(missdeadline_model, type = "perspectives", topics = 6)
plot(missdeadline_model, type = "perspectives", topics = 7)
plot(missdeadline_model, type = "summary", xlim = c(0, 0.3))
summary(missdeadline_model)
summary(missdeadline_effect)

caraccidentinvolve_effect <- estimateEffect(1:4 ~ Sex, caraccidentinvolve_model, meta = meta_caraccidentinvolve, uncertainty = "Local", documents = docs_caraccidentinvolve)
plot(caraccidentinvolve_effect, covariate = "Sex", topics = c(1, 2, 3, 4), model = caraccidentinvolve_model, 
     method = "difference", cov.value1 = "Female", cov.value2 = "Male", xlab = "More Male ... More Female", 
     main = "Event 3: Topic Prevalence", xlim = c(-0.1, 0.1), labeltype = "custom", 
     custom.labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4"))
plot(caraccidentinvolve_model, type = "perspectives", topics = 1)
plot(caraccidentinvolve_model, type = "perspectives", topics = 2)
plot(caraccidentinvolve_model, type = "perspectives", topics = 3)
plot(caraccidentinvolve_model, type = "perspectives", topics = 4)
plot(caraccidentinvolve_model, type = "summary", xlim = c(0, 0.5))
summary(caraccidentinvolve_model)
summary(caraccidentinvolve_effect)

cheatexam_effect <- estimateEffect(1:6 ~ Sex, cheatexam_model, meta = meta_cheatexam, uncertainty = "Local", documents = docs_cheatexam)
plot(cheatexam_effect, covariate = "Sex", topics = c(1, 2, 3, 4, 5, 6), model = cheatexam_model, 
     method = "difference", cov.value1 = "Female", cov.value2 = "Male", xlab = "More Male ... More Female", 
     main = "Event 4: Topic Prevalence", xlim = c(-0.1, 0.1), labeltype = "custom", 
     custom.labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6"))
plot(cheatexam_model, type = "perspectives", topics = 1)
plot(cheatexam_model, type = "perspectives", topics = 2)
plot(cheatexam_model, type = "perspectives", topics = 3)
plot(cheatexam_model, type = "perspectives", topics = 4)
plot(cheatexam_model, type = "perspectives", topics = 5)
plot(cheatexam_model, type = "perspectives", topics = 6)
plot(cheatexam_model, type = "summary", xlim = c(0, 0.4))
summary(cheatexam_model)
summary(cheatexam_effect)

getfired_effect <- estimateEffect(1:6 ~ Sex, getfired_model, meta = meta_getfired, uncertainty = "Local", documents = docs_getfired)
plot(getfired_effect, covariate = "Sex", topics = c(1, 2, 3, 4, 5, 6), model = getfired_model, 
     method = "difference", cov.value1 = "Female", cov.value2 = "Male", xlab = "More Male ... More Female", 
     main = "Event 5: Topic Prevalence", xlim = c(-0.2, 0.2), labeltype = "custom", 
     custom.labels = c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6"))
plot(getfired_model, type = "perspectives", topics = 1)
plot(getfired_model, type = "perspectives", topics = 2)
plot(getfired_model, type = "perspectives", topics = 3)
plot(getfired_model, type = "perspectives", topics = 4)
plot(getfired_model, type = "perspectives", topics = 5)
plot(getfired_model, type = "perspectives", topics = 6)
plot(getfired_model, type = "summary", xlim = c(0, 0.4))
summary(getfired_model)
summary(getfired_effect)

topicQuality(noticket_model, documents = docs_noticket, xlab = "Semantic Coherence", ylab = "Exclusivity", labels = 1:10, M = 10)

# Statistical Testing: Topic Content with n-gram differentiation test

## function for n-gram differentiation test (between-test)

require(quanteda)
require(data.table)

ngram_diff_between <- function(data
                              , id_var
                              , text_var
                              , splitter_var
                              , splitter_level_1
                              , splitter_level_2
                              , rm_stopwords_bool = TRUE
                              , stem_bool = TRUE
                              , ngram_max
                              , min_doc_freq = 0.05
                              , nboot = 100){
  
  
  var_vec <- c(id_var, text_var, splitter_var)
  data <- as.data.table(data)
  data.df <- data.frame(data[, ..var_vec])
  names(data.df) <- c('id', 'text', 'splitter')
  data.df$doc_id <- paste0('text', 1:nrow(data.df))
  
  t <- tokens(tolower(data.df$text)
             , remove_punct = T
             , remove_numbers = F)
  
  if(rm_stopwords_bool == T){
    t_ <- tokens_select(t, pattern = stopwords("en"), selection = "remove")
  } else {
    t_ <- t
  }
  
  
  ngrams_step_1 <- dfm(tokens_ngrams(t_
                                    , n = 1:ngram_max
                                    , skip = 0))
  
  if(stem_bool == T){
    ngrams_step_2 <- dfm_wordstem(x = ngrams_step_1)
  } else {
    ngrams_step_2 <- ngrams_step_1
  }
  
  
  ngrams_step_3 <- dfm_trim(ngrams_step_2
                           , min_docfreq = min_doc_freq
                           , docfreq_type = 'prop')
  
  
  ngram.dt <- setDT(convert(ngrams_step_3, 'data.frame'))
  ngram.dt[, splitter := data.df$splitter]
  
  print('--- finished preprocessing ---')
  
  # run analysis
  list_for_results <- list()
  cols <- names(ngram.dt)[!(names(ngram.dt) %in% c('doc_id', 'splitter'))]
  n_retained_ngrams <- length(cols)
  
  for(i in 1:(length(cols)-1)){
    print(paste0(i, "/", (length(cols)-1), " --> ", cols[i]))
    
    ngram <- cols[i]
    
    min_df <- data.frame(w = ngram.dt[, ..ngram]
                        , SPLIT = ngram.dt[, splitter])
    names(min_df) <- c('w', 'split')
    
    # custom Wilcoxon rank-sum test
    d <- setDT(min_df)
    
    vec_r <- numeric()
    for(j in 1:nboot){
      set.seed(j)
      n_obs <- d[, .N]
      d[, rank := frank(w, ties.method = 'random')]
      ranksum_1 <- sum(d[split == splitter_level_1, rank])
      ranksum_2 <- sum(d[split == splitter_level_2, rank])
      ranksum_full <- n_obs*(n_obs + 1)/2
      
      n_1 <- length(d$rank[d$split == splitter_level_1])
      n_2 <- length(d$rank[d$split == splitter_level_2])
      
      U_1 <- ranksum_1 - ((n_1*(n_1+1))/2)
      U_2 <- ranksum_2 - ((n_1*(n_1+1))/2)
      
      r <- ((2*U_1)/(n_1*n_2))-1
      
      vec_r[j] <- r
    }
    
    # to output
    out_1 <- data.frame('ngram' = ngram
                       , 'M1' = round(mean(min_df$w[min_df$split == splitter_level_1]), 2)
                       , 'SD1' = round(sd(min_df$w[min_df$split == splitter_level_1]), 2)
                       , 'M2' = round(mean(min_df$w[min_df$split == splitter_level_2]), 2)
                       , 'SD2' = round(sd(min_df$w[min_df$split == splitter_level_2]), 2)
                       , 'U_r_boot_M' = round(mean(vec_r), 4)
                       , 'U_r_boot_SD' = round(sd(vec_r), 4)
    )
    
    list_for_results[[i]] <- out_1
  }
  
  ndt <- rbindlist(list_for_results)
  ndt_ordered <- ndt[order(-abs(U_r_boot_M)), ]
  return(list(results=ndt_ordered, n_retained_ngrams=n_retained_ngrams))
  
}

## Event 1

noticket_contenttest <- ngram_diff_between(data = data_noticket, id_var = 'Participant_id', 
                                           text_var = 'False_event_clean',
                                           splitter_var = 'Sex', splitter_level_1 = 'Female',
                                           splitter_level_2 = 'Male', ngram_max = 1,
                                           rm_stopwords_bool = F,
                                           min_doc_freq = 0.05, stem_bool = F)
head(noticket_contenttest$results, 15)

## Event 2

missdeadline_contenttest <- ngram_diff_between(data = data_missdeadline, id_var = 'Participant_id', 
                                           text_var = 'False_event_clean',
                                           splitter_var = 'Sex', splitter_level_1 = 'Female',
                                           splitter_level_2 = 'Male', ngram_max = 1,
                                           rm_stopwords_bool = F,
                                           min_doc_freq = 0.05, stem_bool = F)
head(missdeadline_contenttest$results, 15)

## Event 3

caraccidentinvolve_contenttest <- ngram_diff_between(data = data_caraccidentinvolve, id_var = 'Participant_id', 
                                           text_var = 'False_event_clean',
                                           splitter_var = 'Sex', splitter_level_1 = 'Female',
                                           splitter_level_2 = 'Male', ngram_max = 1,
                                           rm_stopwords_bool = F,
                                           min_doc_freq = 0.05, stem_bool = F)
head(caraccidentinvolve_contenttest$results, 30)

## Event 4

cheatexam_contenttest <- ngram_diff_between(data = data_cheatexam, id_var = 'Participant_id', 
                                           text_var = 'False_event_clean',
                                           splitter_var = 'Sex', splitter_level_1 = 'Female',
                                           splitter_level_2 = 'Male', ngram_max = 1,
                                           rm_stopwords_bool = F,
                                           min_doc_freq = 0.05, stem_bool = F)
head(cheatexam_contenttest$results, 25)

## Event 5

getfired_contenttest <- ngram_diff_between(data = data_getfired, id_var = 'Participant_id', 
                                           text_var = 'False_event_clean',
                                           splitter_var = 'Sex', splitter_level_1 = 'Female',
                                           splitter_level_2 = 'Male', ngram_max = 1,
                                           rm_stopwords_bool = F,
                                           min_doc_freq = 0.05, stem_bool = F)
head(getfired_contenttest$results, 15)