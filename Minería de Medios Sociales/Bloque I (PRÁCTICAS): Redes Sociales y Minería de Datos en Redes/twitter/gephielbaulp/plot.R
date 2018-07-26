require(ggplot2)

data <- read.csv("./data.csv", header = T)
plot(table(data$clustering), type = "ph")

# dev.print(png, "./imgs/clustering.png", width = 600)

ggplot2::autoplot(data$betweenesscentrality, data$eigencentrality)

ggplot2::ggplot(data, aes(betweenesscentrality, eigencentrality, label = Label)) +
    geom_point() +
    geom_text(aes(label = ifelse(betweenesscentrality > .04 | eigencentrality > .75,
                                 as.character(Label), '')),
              hjust=-0.05,vjust=0)
# dev.print(png, "./imgs/eigenvsbt.png", width = 1024)
