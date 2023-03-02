


# setup -------------------------------------------------------------------
source(here::here("r", "libraries.r"))
dk12 <- here::here("data", "k12")


# get data ----------------------------------------------------------------

url <- "https://nces.ed.gov/programs/digest/d21/tables/xls/tabn235.20.xls"
fpath <- path(dk12, path_file(url))

download.file(url, fpath, mode="wb")

df1 <- read_excel(fpath, range="a7:o58", col_names=FALSE)

# State or jurisdiction	Total (in thousands)	Federal	State	Local (including intermediate sources below the state level)
# Amount (in thousands)	Per pupil	Percent of total	Amount (in thousands)	Percent of total	Amount (in thou- sands)1	Percent of total	Property taxes	Other public revenue	Private2
# Amount (in thousands)	Percent of total	Amount (in thousands)	Percent of total	Amount (in thousands)	Percent of total
# 1	2	3	4	5	6	7	8	9	10	11	12	13	14	15
# United States	$764,716,225	$60,338,949	$1,193	7.9	$356,997,353	46.7	$347,379,923	45.4	$279,154,855	36.5	$56,272,806	7.4	$11,952,262	1.6

df2 <- df1 |> 
  select(stname=1, 
         totrev=2,
         frev=3, fpp=4, fpct=5,
         srev=6, spct=7,
         lrev=8, lpct=9,
         lptax=10, lptaxpct=11) |> 
  mutate(across(-stname, as.numeric)) |> 
  mutate(pupils=frev / fpp)

df3 <- df2 |> 
  mutate(rtpp=totrev / pupils,
         lrpp=lrev / pupils)







# finance data ------------------------------------------------------------


dfin <- r"(E:\R_projects\rural_schools\data)"

kbasic <- readRDS(path(dfin, "basic.rds"))

finwide <- readRDS(path(dfin, "finwide.rds"))
count(finwide, year)
ns(finwide)

fin2 <- finwide |> 
  filter(year==2018) |> 
  left_join(kbasic |> select(districtid, dname),
            by = join_by(districtid))

skim(fin2 |> select(rev_pupil))
skim(fin2 |> select(exp_pupil))

ns(fin2)
fin2 |> select(exp_pupil)

fin3 <- fin2 |> 
  select(year, districtid, dname, dcaadm, lrev, exp_pupil, av_twpu, inc_twpu, cwr) |> 
  mutate(lrevpp=lrev / dcaadm)
skim(fin3)

fin3 |> 
  filter(dcaadm >= 200) |> 
  filter(lrevpp<=60e3, exp_pupil <= 60e3) |> 
  ggplot(aes(lrevpp, exp_pupil)) +
  geom_point() +
  theme_minimal()
  # geom_abline(slope=1)

fin3 |> 
  filter(dcaadm >= 200) |> 
  # filter(av_twpu <= 1.5e7, exp_pupil <= 60e3) |>
  filter(av_twpu <= 5e6, exp_pupil <= 60e3) |> 
  ggplot(aes(av_twpu, exp_pupil)) +
  geom_point() +
  theme_minimal()

fin3 |> 
  filter(dcaadm >= 200) |> 
  filter(cwr <= 13, exp_pupil <= 60e3) |>
  # filter(av_twpu <= 5e6, exp_pupil <= 60e3) |> 
  ggplot(aes(cwr, exp_pupil)) +
  geom_point() +
  theme_minimal()




unequal <- fin3 |> 
  filter(dcaadm >= 200) |> # leaves 657 districts
  summarise(pmin=min(exp_pupil),
            p01=pany(exp_pupil, .01),
            p05=pany(exp_pupil, .05),
            p10=pany(exp_pupil, .1),
            p25=p25(exp_pupil),
            p50=p50(exp_pupil),
            p75=p75(exp_pupil),
            p90=pany(exp_pupil, .9),
            p95=pany(exp_pupil, .95),
            p99=pany(exp_pupil, .99),
            pmax=max(exp_pupil)) |> 
  mutate(p7525=p75 / p25,
         p9010=p90 / p10)
unequal

unequal |> 
  select(-p7525, -p9010) |> 
  pivot_longer(cols=everything()) |> 
  add_row(name="zero", value=NA_real_, .before=1) |> 
  mutate(x="1",
         row=row_number(),
         x=ifelse(row==1, NA_character_, "1"),
         ptile=c(NA_real_, 0, .1, .25, .5, .75, .9, 1),
         proportion=lead(ptile) - ptile,
         group=c(NA_real_, 1, 1, 2, 3, 3, 4, 4) |> as.character()) |> 
  ggplot(aes(x=x, y=value, fill=group)) +
  geom_col(na.rm=FALSE) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16)



pdata <- unequal |> 
  select(-p7525, -p9010) |> 
  pivot_longer(cols=everything()) |> 
  mutate(x="1",
         row=row_number(),
         ptile=c(0, .01, .05, .1, .25, .5, .75, .9, .95,  .99, 1),
         proportion=lead(ptile) - ptile)
pdata

pdata |> 
  mutate(group=c(1, 1, 2, 3, 4, 4, 4, 5, 6) |> as.character()) |> 
  ggplot(aes(x=x, y=value, fill=group)) +
   geom_col(na.rm=FALSE)


# ,
#          group=c(NA_real_, 1, 2, 3, 3, 4, 4) |> as.character()) |> 
#   ggplot(aes(x=x, y=value, fill=group)) +
#   geom_col(na.rm=FALSE)



ncestry <- data.frame(Race = c("European", "African American", "Asian", "Hispanic", "Other"), 
                      Proportion = c(40, 30, 10, 15, 5))

Ancestry <- Ancestry %>% 
  mutate(Year = "2006")

ggplot(Ancestry, aes(x = Year, y = Proportion, fill = Race)) +
  geom_col() +
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)



ggplot(data = TTM, aes(x = Type.of.Behavior, y = Sample.Size, fill = Stage.of.Change)) + 
  geom_bar()

ggplot(df, aes(x = x, y = y, fill = group)) + 
  geom_bar(stat = "identity")


