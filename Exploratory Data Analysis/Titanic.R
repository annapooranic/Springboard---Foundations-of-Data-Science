
# To check the structure of titanic
str(titanic)

# ggplot() for the first instruction
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")

# to add facet_grid() layer
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")+
  facet_grid(.~Survived)

# Definition of an object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# Adding the position object from preious instruction to the plot

ggplot(titanic, aes(x = Pclass,y=Age, color = Sex)) +
  geom_point(size=3,alpha=0.5,position = posn.jd)+
  facet_grid(.~Survived)
