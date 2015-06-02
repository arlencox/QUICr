library(plyr)
options(width=200)

# read in the data file
t <- read.csv("data/results_sdsl.csv", header=TRUE, colClasses=c("factor", "factor", "character", "factor", "factor", "character", "factor", "character", "character"))
# fix the dates
t$Date <- as.POSIXct(gsub("T", " ", t$Date), tz="GMT")
# fix the Pass and Total columns
t$Pass <- as.integer(t$Pass)
t$Total <- as.integer(t$Total)
# fix the Time column
t$Time <- as.numeric(t$Time)

# extract the most recent configuration
recent_date <- max(t$Date)

# extract machine id and build id
recent_record <- t[t$Date == recent_date, ]
machine_id <- recent_record$Machine.ID
build_id <- recent_record$Build.ID

# extract the data replacing the frame
t <- t[t$Machine.ID == machine_id && t$Build.ID == build_id, -c(1,2,3) ]

# run averages
run_time <- ddply(.data=t, c("Config", "Test"), summarise, Time = mean(Time), Pass = mean(Pass), Total = mean(Total))

result <- ddply(.data=run_time, c("Config"), summarise, Time = sum(Time), Pass = sum(Pass), Total = sum(Total))
print.data.frame(result)

# read in the second data file
t <- read.csv("data/results_strace.csv", header=TRUE, colClasses=c("factor", "factor", "character", "factor", "factor", "character", "factor", rep("character", 4 * 2)))
# fix the dates
t$Date <- as.POSIXct(gsub("T", " ", t$Date), tz="GMT")
# fix the Pass and Total columns
t[,8:15] <- mapply(as.integer, t[,8:15])
# fix the Time column
t$Time <- as.numeric(t$Time)

# extract the most recent configuration
recent_date <- max(t$Date)

# extract machine id and build id
recent_record <- t[t$Date == recent_date, ]
machine_id <- recent_record$Machine.ID
build_id <- recent_record$Build.ID

# extract the data replacing the frame
t <- t[t$Machine.ID == machine_id && t$Build.ID == build_id, -c(1,2,3) ]

# run averages
run_time <- ddply(
    .data=t,
    c("Config", "Test"),
    summarise,
    Time = mean(Time),
    SAT.Pass = mean(SAT.Pass),
    SAT.Count = mean(SAT.Count),
    LE.Pass = mean(LE.Pass),
    LE.Count = mean(LE.Count),
    Top.Pass = mean(Top.Pass),
    Top.Count = mean(Top.Count),
    Bot.Pass = mean(Bot.Pass),
    Bot.Count = mean(Bot.Count)
)

result <- ddply(
    .data=run_time,
    c("Config"),
    summarise,
    Time = sum(Time),
    SAT.Pass = sum(SAT.Pass),
    SAT.Count = sum(SAT.Count),
    LE.Pass = sum(LE.Pass),
    LE.Count = sum(LE.Count),
    Top.Pass = sum(Top.Pass),
    Top.Count = sum(Top.Count),
    Bot.Pass = sum(Bot.Pass),
    Bot.Count = sum(Bot.Count)
)

print.data.frame(result)
