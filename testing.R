library(stm)

## Ingest: reading and processing text data

data <- read.csv("Embedded_lies_manprep.csv", sep = ";")
processed <- textProcessor(data$False_event_clean, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

## Prepare: associating text with metadata

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)

## Estimate: estimating the structural topic model

embedliesPrevFit <- stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence = ~Sex, max.em.its = 75, data = out$meta, init.type = "Spectral")

## Evaluate: Model selection and search

embedliesSelect <- selectModel(out$documents, out$vocab, K = 20, prevalence = ~Sex, max.em.its = 75, data = out$meta, runs = 20, seed = 8458159)
plotModels(embedliesSelect, pch = c(1, 2, 3, 4), legend.position = "bottomright")
selectedmodel <- embedliesSelect$runout[[3]]
storage <- searchK(out$documents, out$vocab, K = c(7, 10), prevalence = ~Sex, data = meta)

## Understand: Interpreting the STM by plotting and inspecting results

labelTopics(embedliesPrevFit, c(6, 13, 18))

prep <- estimateEffect(1:20 ~Sex, embedliesPrevFit, meta = out$meta, uncertainty = "Global")

## Visualize: Presenting STM results

# Summary visualization
plot(embedliesPrevFit, type = "summary", xlim = c(0, 0.3))

# Metadata/topic relationship visualization
plot(prep, covariate = "Sex", topics = c(6, 13, 18), model = embedliesPrevFit, method = "difference", cov.value1 = "Male", cov.value2 = "Female", xlab = "More Male ... More Female", main = "Effect of Male vs. Female", xlim = c(-0.1, 0.1), labeltype = "custom", custom.labels = c("Topic 6", "Topic 13", "Topic 18"))

# Topical Content
embedliesContent <- stm(out$documents, out$vocab, K = 20, prevalence = ~Sex, content = ~Sex, max.em.its = 75, data = out$meta, init.type = "Spectral")
# Need to clean dataset still!
plot(embedliesContent, type = "perspectives", topics = 10)
plot(embedliesPrevFit, type = "perspectives", topics = c(16, 18))

# Plotting covariate interactions, needs more work to apply to current dataset
embedliesInteraction <- stm(out$documents, out$vocab, K = 20, prevalence =~ Sex * Language, max.em.its = 75, data = out$meta, init.type = "Spectral")
prep <- estimateEffect(c(16) ~ Sex * Language, embedliesInteraction, + metadata = out$meta, uncertainty = "None")
plot(prep, covariate = "Sex", model = embedliesInteraction, + method = "continuous", xlab = "Sex", moderator = "Language", moderator.value = "Male", linecol = "blue", ylim = c(0, 0.12), printlegend = FALSE)
plot(prep, covariate = "Sex", model = embedliesInteraction, method = "continuous", xlab = "Sex", moderator = "Language", moderator.value = "Female", linecol = "red", add = TRUE, printlegend = FALSE)
legend(0, 0.06, c("Male", "Female"), lwd = 2, col = c("blue", "red"))
