

# loads -------------------------------------------------------------------

dhere <- here::here("budget_presentation")
source(fs::path(dhere, "libraries.r"))

fn <- "SpendingData.xlsx"
# Note to self: These data are disbursements, not appropriations.

# get budget data ---------------------------------------------------------

df1 <- read_excel(path(dhere, fn))
glimpse(df1)

df2 <- df1 |> 
  rename(func=Function,
         agency=Agency,
         fundtype=`Fund Type`,
         fpcat=`FP Category`,
         fund=Fund,
         subfund=Subfund,
         subfname=`Subfund Name`) |> 
  pivot_longer(-c(1:7), names_to = "sfy") |> 
  mutate(valtype=case_when(str_detect(sfy, "Estimate") ~ "estimate",
                           str_detect(sfy, "Actual") ~ "actual",
                           TRUE ~ "unknown"),
         sfy=str_sub(sfy, 1, 4) |> as.integer() + 1) |> 
  arrange(sfy)
glimpse(df2)

count(df2, valtype)
count(df2, sfy)
count(df2, func)
count(df2, agency) # 147
count(df2, fundtype) # 6
count(df2, fpcat) # 8
count(df2, fund) # 114
count(df2, subfund, subfname) # 700+

# General Fund: Unrestricted Receipts
# State Operating Funds: General Fund, Debt Service Funds, 
#    State Special Revenue Funds
# State Funds: State Operating Funds Plus Capital Funds
# All Funds: State Funds Plus Federal Funds

# table: SOF, CPF, FedOpAid, AllGovtFunds

vsof <- c("General Fund", "Debt Service Funds", "Special Revenue Funds - Other")
# sof <- expression(fundtype %in% vsof)
vcpf <- c("Capital Projects Funds - Federal", "Capital Projects Funds - Other")
vfedop <- c("Special Revenue Funds - Federal")


flevs <- c("sof", "cpf", "fedop", "allfunds")
flabs <- c("TOTAL STATE OPERATING FUNDS",
           "Capital Projects (State & Federal)",
           "Federal Operating Aid",
           "TOTAL ALL GOVERNMENTAL FUNDS")

tabdata <- df2 |>
  filter(sfy %in% 2021:2023) |> 
  mutate(sof=value * fundtype %in% vsof,
         cpf=value * fundtype %in% vcpf,
         fedop=value * fundtype %in% vfedop,
         allfunds=value) |> 
  summarise(across(c(sof, cpf, fedop, allfunds), ~sum(.x, na.rm=TRUE)), .by=sfy) |> 
  pivot_longer(-sfy) |> 
  pivot_wider(names_from = sfy) |> 
  mutate(change=`2023` - `2022`,
         pch=change / `2022`,
         namef=factor(name, 
                      levels=flevs,
                      labels=flabs)) |> 
  relocate(namef, .after = name)
tabdata  

tab <- tabdata |> 
  select(-name, -`2021`) |> 
  gt() |> 
  tab_header(
    title = "NYS Disbursements by State Fiscal Year",
    subtitle = "Millions of dollars"
    ) |> 
  cols_label(namef="Fund group",
             `2022`=html("2021-22<br>(Actuals)"),
             `2023`=html("2022-23<br>(Projected)"),
             pch="% change") |> 
  cols_align(align="left", columns=namef) |> 
  fmt_number(columns=c(`2022`, `2023`, change),
             scale_by = 1e-3,
             decimals = 0) |> 
  fmt_currency(columns=c(`2022`, `2023`, change),
               rows=c(1, nrow(tabdata)),
             scale_by = 1e-3,
             decimals = 0) |> 
  fmt_percent(columns=pch,
              decimals=1) |> 
  gt_highlight_rows(rows = nrow(tabdata),
                    fill="grey97") |> 
  tab_source_note(source_note="Source: NY Open Budget (https://openbudget.ny.gov/spendingForm.html)")
tab  


library(tidycensus)
test <- get_acs(geography = "state legislative district (lower chamber)",
                variables = "B05002_013E", year = 2012, state = "NY")

test <- get_decennial(geography = "state legislative district (lower chamber)",
                      variables = c("P005003", "P005004", "P005006", "P004003"), year = 2010, state = "NY")
