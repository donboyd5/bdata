# btools_states.R
# Don Boyd
# 4/29/2015

# Create stcodes data file that has various state codes available


stabbr <- c('US','AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
            'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY',
            'PR','VI')
stname <- c('United States','Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','District of Columbia',
            'Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland',
            'Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey',
            'New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island',
            'South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming',
            'Puerto Rico','Virgin Islands')
stfips <- c('00','01','02','04','05','06','08','09','10','11','12','13','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30',
            '31','32','33','34','35','36','37','38','39','40','41','42','44','45','46','47','48','49','50','51','53','54','55','56',
            '72','78')
stcen <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27',
           '28','29','30','31','32','33','34','35','36','37','38','39','40','41','42','43','44','45','46','47','48','49','50','51',
           '72','78') # I made this up for territories

stcodes<-data.frame(stabbr, stfips, stcen, stname)

# BEA Regions
neng.b <- c("CT", "MA", "ME", "NH", "RI", "VT")
mdatl.b <- c("DE", "DC", "MD", "NJ", "NY", "PA") # BEA calls this mideast but Lucy uses Mid Atlantic
glr.b <- c("IL", "IN", "MI", "OH", "WI")
plr.b <- c("IA", "KS", "MN", "MO", "NE", "ND", "SD")
ser.b <- c("AL", "AR", "FL", "GA", "KY", "LA", "MS", "NC", "SC", "TN", "VA", "WV")
swr.b <- c("AZ", "NM", "OK", "TX")
rmr.b <- c("CO", "ID", "MT", "UT", "WY")
fwr.b <- c("AK", "CA", "HI", "NV", "OR", "WA")


stcodes$beargn[stcodes$stabbr %in% neng.b] <- "neng"
stcodes$beargn.name[stcodes$stabbr %in% neng.b] <- "New England"

stcodes$beargn[stcodes$stabbr %in% mdatl.b] <- "mdatl"
stcodes$beargn.name[stcodes$stabbr %in% mdatl.b] <- "Mid Atlantic"

stcodes$beargn[stcodes$stabbr %in% glr.b] <- "glr"
stcodes$beargn.name[stcodes$stabbr %in% glr.b] <- "Great Lakes"

stcodes$beargn[stcodes$stabbr %in% plr.b] <- "plr"
stcodes$beargn.name[stcodes$stabbr %in% plr.b] <- "Plains"

stcodes$beargn[stcodes$stabbr %in% ser.b] <- "ser"
stcodes$beargn.name[stcodes$stabbr %in% ser.b] <- "Southeast"

stcodes$beargn[stcodes$stabbr %in% swr.b] <- "swr"
stcodes$beargn.name[stcodes$stabbr %in% swr.b] <- "Southwest"

stcodes$beargn[stcodes$stabbr %in% rmr.b] <- "rmr"
stcodes$beargn.name[stcodes$stabbr %in% rmr.b] <- "Rocky Mountain"

stcodes$beargn[stcodes$stabbr %in% fwr.b] <- "fwr"
stcodes$beargn.name[stcodes$stabbr %in% fwr.b] <- "Far West"

stcodes$beargn[stcodes$stabbr %in% c("US")] <- "usr"
stcodes$beargn.name[stcodes$stabbr %in% c("US")] <- "United States"

stcodes

devtools::use_data(stcodes, overwrite=TRUE)

