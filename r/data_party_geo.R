


# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
memdata <- r"(E:\data\legislatures_and_politics\ElectedOfficials.csv)"

# get data ---------------------------------------------------------------
df1 <- read_csv(memdata)
glimpse(df1)

df2 <- df1 |> 
  lcnames()
glimpse(df2)
count(df2, office, party)

count(df2 |> filter(str_detect_any(office, c("Assembly", "Senator"))), office, party)


df2 |> filter(party=="RAU - Rise and Unite")


