data <- read.csv("Embedded_lies_manprep.csv", sep = ";")

# Check general count
table(data$Sex)
table(data$Valence_event)

# Remove CONSENT_REVOKED, DATA_EXPIRED, and Prefer Not To Say
data <- data[!(data$Sex %in% c("CONSENT_REVOKED", "DATA_EXPIRED", "Prefer not to say")),]

# Check removal
table(data$Sex)

# Check valence per gender
table(data$Sex, data$Valence_event)

# Check count per event type
table(data$Event, data$Sex)

# Check valence per event type
table(data$Event, data$Valence_event)

# Create dataframes per event_type being considered
data_noticket <- data[data$Event == "Taking the bus/train without the ticket",]
data_missdeadline <- data[data$Event == "Missing a deadline at work because of bad organisation",]
data_caraccidentinvolve <- data[data$Event == "Being involved in a car accident",]
data_cheatexam <- data[data$Event == "Cheating on an exam",]
data_getfired <- data[data$Event == "Getting fired",]

# Check valence and gender balence per selected event type
table(data_noticket$Sex, data_noticket$Valence_event)
table(data_missdeadline$Sex, data_missdeadline$Valence_event)
table(data_caraccidentinvolve$Sex, data_caraccidentinvolve$Valence_event)
table(data_cheatexam$Sex, data_cheatexam$Valence_event)
table(data_getfired$Sex, data_getfired$Valence_event)