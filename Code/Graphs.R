# Pickle file BMR distribution
bg_mutability[which(bg_mutability$Mutability<=0.00005),] %>% 
  ggplot(.,aes(x = .$Mutability)) +
  geom_histogram(fill="#0072B2") + 
  ylab("BMR Frequency") + 
  xlab("BMR / Expected Mutability") + 
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12)) +
  geom_vline(aes(xintercept = mean(bg_mutability$Mutability)),col='red',size=0.5) +
  geom_vline(aes(xintercept = median(bg_mutability$Mutability)),col="green",size=0.5)


# No. of Gene_Codon for each Frequency in Genie
# Bar chart
freq_cat %>% 
  select(Category, Count_No) %>%
  group_by(Category) %>%
  summarise(
    Total_No = sum(Count_No)
  ) %>% 
  mutate(
    percentage = paste0(round(Total_No / sum(Total_No) * 100, 1), "%")
  ) %>%
  arrange(., desc(Total_No)) %>%
  ggplot(.,aes(x = Category, y = Total_No)) +
  geom_bar(stat= 'identity', fill="#0072B2", width = 0.7,) + 
  xlab("(a)") + 
  ylab("No. of Codons") + 
  geom_text(aes(label= paste(Total_No," (",percentage,")")), vjust=-0.3, size=3.2) +
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,))


# No. of Gene_Codon for each Frequency in TCGA
# Bar chart
freq_cat_tcga %>% 
  select(Category, Count_No) %>%
  group_by(Category) %>%
  summarise(
    Total_No = sum(Count_No)
  ) %>%
  mutate(
    percentage = paste0(round(Total_No / sum(Total_No) * 100, 1), "%")
  ) %>%
  arrange(., desc(Total_No)) %>%
  ggplot(.,aes(x = Category, y = Total_No)) +
  geom_bar(stat= 'identity', fill="#0072B2", width = 0.7,) + 
  xlab("(b)") + 
  ylab("No. of Codons") + 
  geom_text(aes(label= paste(Total_No," (",percentage,")")), vjust=-0.3, size=3.2) +
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12,))


# Binomial model plot
# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,50,by = 1)
# Create the binomial distribution.
y <- dbinom(x,50,0.5)
binom_df = data.frame(x=x,prob=y)

binom_df %>% 
  ggplot(.,aes(x=factor(x), y = prob)) +
  geom_point(size=2,color="blue",shape=1) +
  geom_vline(aes(xintercept = 38),col='red',size=0.3) +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_text(size=12,face="bold")) +
  xlab("No. of samples") + 
  ylab("Probability")  


# BH vs BY
ggplot(data=df, aes(x=Dataset, y=SigNo, fill=Method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=SigNo), vjust=-0.3, position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired") +
  ggtitle("No. of Potential Driver Mutations")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_blank()) 


