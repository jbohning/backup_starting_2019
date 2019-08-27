#Some examples to dealing with strings in r



#########################################################
#BASICS
#A few ways to enter quotation marks and apostraphes inside a string
line1 <- "The table was a large one, but the three were all crowded together at one corner of it:"
line2 <- '"No room! No room!" they cried out when they saw Alice coming.'
line3 <- "\"There's plenty of room!\" said Alice indignantly, and she sat down in a large arm-chair at one end of the table."

# Putting lines in a vector
lines <- c(line1, line2, line3)
# Print lines
print(lines)
# Use writeLines() on lines
writeLines(lines)
# Write lines with a space separator
writeLines(lines,sep=" ")
# Use writeLines() on the string "hello\n\U1F30D" Note: the last part should
#produce an emoji globe
writeLines("hello\n\U1F30D")

# Should display: To have a \ you need \\
writeLines("To have a \\ you need \\\\")
# Should display: 
# This is a really 
# really really 
# long string
writeLines("This is a really \nreally really \nlong string")
# Use writeLines() with 
# "\u0928\u092e\u0938\u094d\u0924\u0947 \u0926\u0941\u0928\u093f\u092f\u093e" (You just said "Hello World" in Hindi!)
writeLines("\u0928\u092e\u0938\u094d\u0924\u0947 \u0926\u0941\u0928\u093f\u092f\u093e")


#Number formats:
# Some vectors of numbers
percent_change  <- c(4, -1.91, 3.00, -5.002)
income <-  c(72.19, 1030.18, 10291.93, 1189192.18)
p_values <- c(0.12, 0.98, 0.0000191, 0.00000000002)
# Format c(0.0011, 0.011, 1) with digits = 1
format(c(0.0011, 0.011, 1),digits=1)
# Format c(1.0011, 2.011, 1) with digits = 1
format(c(1.0011, 2.011, 1),digits=1)
# Format percent_change to one place after the decimal point
format(percent_change,digits=2)
# Format income to whole numbers
format(income, digits=2)
# Format p_values in fixed format
format(p_values,scientific = FALSE)

formatted_income <- format(income, digits = 2)
# Print formatted_income
print(formatted_income)
# Call writeLines() on the formatted income
writeLines(formatted_income)
# Define trimmed_income
trimmed_income<-format(income,digits=2,trim=TRUE)
# Call writeLines() on the trimmed_income
writeLines(trimmed_income)
# Define pretty_income
pretty_income<-format(income,digits=2,big.mark=",")
# Call writeLines() on the pretty_income
writeLines(pretty_income)


# From the format() exercise
x <- c(0.0011, 0.011, 1)
y <- c(1.0011, 2.011, 1)
# formatC() on x with format = "f", digits = 1
formatC(x,format="f",digits=1)
# formatC() on y with format = "f", digits = 1
formatC(y,format="f",digits=1)
# Format percent_change to one place after the decimal point
formatC(percent_change,format="f",digits=1)
# percent_change with flag = "+"
formatC(percent_change,flag="+",format="f",digits=1)
# Format p_values using format = "g" and digits = 2
formatC(p_values,format="g",digits=2)



# Add $ to pretty_income
paste("$",pretty_income,sep="")
# Add % to pretty_percent
#paste(pretty_percent,"%",sep="")
# Create vector with elements like 2010: +4.0%`
#year_percent <- paste(years,": ",pretty_percent,"%",sep="")
# Collapse all years into single string
#paste(year_percent,collapse=",")

# Define the names vector
income_names <- c("Year 0", "Year 1", "Year 2", "Project Lifetime")
# Create pretty_income
pretty_income <- format(income,digits=2,big.mark=",")
# Create dollar_income
dollar_income <- paste0("$",pretty_income)
# Create formatted_names
formatted_names <- format(income_names,justify="right")
# Create rows
rows<-paste(formatted_names,dollar_income,sep="   ")
# Write rows
writeLines(rows)


# Randomly sample 3 toppings
toppings<-c("cheese","onions","anchovies","mushrooms","sausage")
set.seed(123)
my_toppings <- sample(toppings, size = 3)
# Print my_toppings
print(my_toppings)
# Paste "and " to last element: my_toppings_and
my_toppings_and <- paste0(c("","","and "),my_toppings)
# Collapse with comma space: these_toppings
these_toppings <- paste(my_toppings_and,collapse=", ")
# Add rest of sentence: my_order
my_order <- paste0("I want to order a pizza with ", these_toppings,".")
# Order pizza with writeLines()
writeLines(my_order)






#########################################################
#Introduction to stringr
library(stringr)
library(babynames)

my_toppings <- c("cheese", NA, NA)
my_toppings_and <- paste(c("", "", "and "), my_toppings, sep = "")
# Print my_toppings_and
print(my_toppings_and)
# Use str_c() instead of paste(): my_toppings_str
my_toppings_str <- str_c(c("","","and "),my_toppings)
# Print my_toppings_str
print(my_toppings_str)
# paste() my_toppings_and with collapse = ", "
paste(my_toppings_and,collapse=", ")
# str_c() my_toppings_str with collapse = ", "
str_c(my_toppings_str,collapse=", ")


library(stringr)
library(babynames)
library(dplyr)
# Extracting vectors for boys' and girls' names
babynames_2014 <- filter(babynames, year == 2014)
boy_names <- filter(babynames_2014, sex == "M")$name
girl_names <- filter(babynames_2014, sex == "F")$name
# Take a look at a few boy_names
head(boy_names)
# Find the length of all boy_names
boy_length <- str_length(boy_names)
# Take a look at a few lengths
head(boy_length)
# Find the length of all girl_names
girl_length <- str_length(girl_names)
# Find the difference in mean length
mean(girl_length) - mean(boy_length)
# Confirm str_length() works with factors
head(str_length(factor(boy_names)))


# Extract first letter from boy_names
boy_first_letter <- str_sub(boy_names,1,1)
# Tabulate occurrences of boy_first_letter
table(boy_first_letter)
# Extract the last letter in boy_names, then tabulate
boy_last_letter <- str_sub(boy_names,-1,-1)
table(boy_last_letter)
# Extract the first letter in girl_names, then tabulate
girl_first_letter <- str_sub(girl_names,1,1)
table(girl_first_letter)
# Extract the last letter in girl_names, then tabulate
girl_last_letter <- str_sub(girl_names,-1,-1)
table(girl_last_letter)


# Look for pattern "zz" in boy_names
contains_zz <- str_detect(boy_names,pattern=fixed("zz"))
# Examine str() of contains_zz
str(contains_zz)
# How many names contain "zz"?
sum(contains_zz)
# Which names contain "zz"?
boy_names[contains_zz]


# Look for pattern "zz" in boy_names
contains_zz <- str_detect(boy_names,pattern=fixed("zz"))
# Examine str() of contains_zz
str(contains_zz)
# How many names contain "zz"?
sum(contains_zz)
# Which names contain "zz"?
boy_names[contains_zz]
# Which rows in boy_df have names that contain "zz"?
#boy_df[contains_zz,]


# Find boy_names that contain "zz"
str_subset(boy_names,pattern=fixed("zz"))
# Find girl_names that contain "zz"
str_subset(girl_names,pattern=fixed("zz"))
# Find girl_names that contain "U"
starts_U <- str_subset(girl_names,pattern=fixed("U"))
starts_U
# Find girl_names that contain "U" and "z"
str_subset(starts_U,pattern=fixed("z"))


# Count occurrences of "a" in girl_names
number_as <- str_count(girl_names,pattern=fixed("a"))
# Count occurrences of "A" in girl_names
number_As <- str_count(girl_names,pattern=fixed("A"))
# Histograms of number_as and number_As
hist(number_as)
hist(number_As)
# Find total "a" + "A"
total_as <-number_as+ number_As
# girl_names with more than 4 a's
girl_names[total_as>4]


#Splitting Strings
# Some date data
date_ranges <- c("23.01.2017 - 29.01.2017", "30.01.2017 - 06.02.2017")
# Split dates using " - "
split_dates <- str_split(date_ranges,pattern=fixed(" - "))
split_dates
# Split dates with n and simplify specified
split_dates_n <- str_split(date_ranges,pattern=fixed(" - "),n=2,simplify=TRUE)
split_dates_n
# Subset split_dates_n into start_dates and end_dates
start_dates <- split_dates_n[,1]
end_dates<-split_dates_n[,1]
# Split start_dates into day, month and year pieces
str_split(start_dates,pattern=fixed("."),n=3,simplify=TRUE)


both_names <- c("Box, George", "Cox, David")
# Split both_names into first_names and last_names
both_names_split <- str_split(both_names,pattern=fixed(", "),n=2,simplify=TRUE)
# Get first names
first_names <- both_names_split[,2]
# Get last names
last_names <- both_names_split[,1]



# Split lines into words (variables from beginning of this code)
words <- str_split(lines,pattern=fixed(" "))
# Number of words per line
lapply(words,length)
# Number of characters in each word
word_lengths <- lapply(words,str_length)
# Average word length per line
lapply(word_lengths,mean)


# Some IDs
ids <- c("ID#: 192", "ID#: 118", "ID#: 001")
# Replace "ID#: " with ""
id_nums <- str_replace(ids,pattern=fixed("ID#: "),
                       replacement="")
# Turn id_nums into numbers
id_ints <- as.numeric(id_nums)

# Some (fake) phone numbers
phone_numbers <- c("510-555-0123", "541-555-0167")
# Use str_replace() to replace "-" with " "
str_replace(phone_numbers,fixed("-")," ")
# Use str_replace_all() to replace "-" with " "
str_replace_all(phone_numbers,fixed("-")," ")
# Turn phone numbers into the format xxx.xxx.xxxx
str_replace_all(phone_numbers,fixed("-"),".")


#REVIEW
genes<-c("ATTTTAAACCCCCGGGGGGATCGACTTGACTCGTACTTGATG",
         "ACTACTTGGGGGGGGACTCATTACGACATCTACGACACGATC",
         "CGTACTTGACACTTTCATACTGATGTTAAAAACCGGTTTTTT")
# Find the number of nucleotides in each sequence
str_length(genes)
# Find the number of A's occur in each sequence
str_count(genes,pattern=fixed("A"))
# Return the sequences that contain "TTTTTT"
str_subset(genes,pattern=fixed("TTTTTT"))
# Replace all the "A"s in the sequences with a "_"
str_replace_all(genes,pattern=fixed("A"),"_")


#REVIEW
# Define some full names
names <- c("Diana Prince", "Clark Kent")
# Split into first and last names
names_split <- str_split(names," ",n=2,simplify=TRUE)
# Extract the first letter in the first name
abb_first <- str_sub(names_split[,1],1,1)
# Combine the first letter ". " and last name
str_c(abb_first,". ",names_split[,2])

# Use all names in babynames_2014
all_names <- babynames_2014$name
# Get the last two letters of all_names
last_two_letters <- str_sub(all_names,-2,-1)
# Does the name end in "ee"?
ends_in_ee <- str_detect(last_two_letters,fixed("ee"))
# Extract rows and "sex" column
sex <- unlist(babynames_2014[ends_in_ee,"sex"])
# Display result as a table
table(sex)






#########################################################
#Pattern matching with regular expressions
library(rebus)
# Some strings to practice with
x <- c("cat", "coat", "scotland", "tic toc")
# Print END
print(END)
# Run me
str_view(x, pattern = START %R% "c")
# Match the strings that start with "co" 
str_view(x, pattern = START %R% "co")
# Match the strings that end with "at"
str_view(x, pattern = "at" %R% END)
# Match the string that is exactly "cat"
str_view(x, pattern = START %R% "cat" %R% END)
# Match two characters, where the second is a "t"
str_view(x, pattern = ANY_CHAR %R% "t")
# Match a "t" followed by any character
str_view(x, pattern = "t" %R% ANY_CHAR)
# Match two characters
str_view(x, pattern = ANY_CHAR %R% ANY_CHAR)
# Match a string with exactly three characters
str_view(x, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR %R% END)


#Names with qs
pattern <- "q" %R% ANY_CHAR
# Find names that have the pattern
names_with_q <- str_subset(boy_names,pattern=pattern)
# How many names were there?
length(names_with_q)
# Find part of name that matches pattern
part_with_q <- str_extract(boy_names,pattern=pattern)
# Get a table of counts
table(part_with_q)
# Did any names have the pattern more than once?
count_of_q <- str_count(boy_names,pattern=pattern)
# Get a table of counts
table(count_of_q)
# Which babies got these names?
with_q <- str_detect(boy_names,pattern=pattern)
# What fraction of babies got these names?
mean(with_q)

# Match Jeffrey or Geoffrey
whole_names <- or("Jeffrey", "Geoffrey")
str_view(boy_names, pattern = whole_names, match = TRUE)
# Match Jeffrey or Geoffrey, another way
common_ending <- START %R% or("Je","Geo") %R% "ffrey" %R% END
str_view(boy_names, pattern = common_ending, match = TRUE)
# Match with alternate endings
by_parts <- START %R% or("Je","Geo") %R% "ff" %R% or("ry","ery","rey","erey") %R% END
str_view(boy_names, pattern = by_parts, match = TRUE)
# Match names that start with Cath or Kath
ckath <- START %R% or("Cath","Kath")
str_view(girl_names, pattern = ckath, match = TRUE)


#Using character class (so you don't need escape brackets for things like ".")
# Create character class containing vowels
vowels <- char_class("aeiouAEIOU")
# Print vowels
print(vowels)
# See vowels in x with str_view(). Notice how only the first vowel is matched.
str_view(x,vowels)
# See vowels in x with str_view_all(). Now all matches are highlighted.
str_view_all(x,vowels)
# Number of vowels in boy_names
num_vowels <- str_count(boy_names,vowels)
# Number of characters in boy_names
name_length <- str_length(boy_names)
# Calc mean number of vowels
mean(num_vowels)
# Calc mean fraction of vowels per name
mean(num_vowels/ name_length)


#REPETITION
# Vowels from last exercise
vowels <- char_class("aeiouAEIOU")
# See names with only vowels
str_view(boy_names, 
         pattern = exactly(one_or_more(vowels)), 
         match = TRUE)
# Use `negated_char_class()` for matching everything but vowels
not_vowels <- negated_char_class("aeiouAEIOU")
# See names with no vowels
str_view(boy_names, 
         pattern = exactly(one_or_more(not_vowels)), 
         match = TRUE)

#HUNTING FOR PHONE NUMBER
contact<-c("Call me at 555-555-0191","123 Main St","(555) 555 0191","Phone: 555.555.0191 Mobile: 555.555.0192")
# Create a three digit pattern
three_digits <- DGT %R% DGT %R% DGT
# Test it
str_view_all(contact, pattern = three_digits)
#There might be a range of separators, so make a separator pattern, that uses char_class() to combine a character class containing -, ., (, ), and .
separator <- char_class("-.() ")
# Test it
str_view_all(contact, pattern = separator)
#Put together a pattern to match a whole phone number: an optional open parenthesis (OPEN_PAREN), followed by three digits, followed by zero or more separators, followed by three digits followed by zero or more separators, followed by four digits.
three_digits <- DGT %R% DGT %R% DGT
four_digits <- three_digits %R% DGT
separator <- char_class("-.() ")
# Create phone pattern
phone_pattern <- optional(OPEN_PAREN) %R%
    three_digits %R%
    zero_or_more(separator) %R%
    three_digits %R% 
    zero_or_more(separator) %R%
    four_digits
# Test it           
str_view_all(contact, pattern = phone_pattern)
# Extract phone numbers
str_extract(contact,phone_pattern)
#Try using str_extract_all() instead. Can you see the difference? It exports a list
str_extract_all(contact,phone_pattern)

#EXTRACTING AGE AND GENDER FROM ACCIDENT NARRATIVES
narratives<-c(
"19YOM-SHOULDER STRAIN-WAS TACKLED WHILE PLAYING FOOTBALL W/ FRIENDS ",
"31 YOF FELL FROM TOILET HITITNG HEAD SUSTAINING A CHI ",
"ANKLE STR. 82 YOM STRAINED ANKLE GETTING OUT OF BED ",
"TRIPPED OVER CAT AND LANDED ON HARDWOOD FLOOR. LACERATION ELBOW, LEFT. 33 YOF*",
"10YOM CUT THUMB ON METAL TRASH CAN DX AVULSION OF SKIN OF THUMB ",
"53 YO F TRIPPED ON CARPET AT HOME. DX HIP CONTUSION ",
"13 MOF TRYING TO STAND UP HOLDING ONTO BED FELL AND HIT FOREHEAD ON RADIATOR DX LACERATION",
"14YR M PLAYING FOOTBALL; DX KNEE SPRAIN ",
"55YOM RIDER OF A BICYCLE AND FELL OFF SUSTAINED A CONTUSION TO KNEE ",
"5 YOM ROLLING ON FLOOR DOING A SOMERSAULT AND SUSTAINED A CERVICAL STRA IN"
)
# Pattern to match one or two digits
age <- one_or_more(DGT %R% DGT)
# Test it
str_view(narratives, pattern = age)
# Use this pattern
age <- DGT %R% optional(DGT)
# Create a unit pattern that matches an optional space, then one of YO, YR or MO
unit <- or("YO","YR","MO")
# Test pattern with age then units
str_view(narratives, pattern = age %R% unit)
# Use these patterns
age <- DGT %R% optional(DGT)
unit <- optional(SPC) %R% or("YO", "YR", "MO")
# Create a gender pattern that matches an optional space then M or F.
gender <- optional(" ") %R% or("M","F")
# Test pattern with age then units then gender
str_view(narratives, pattern = age %R% unit %R% gender)
age <- DGT %R% optional(DGT)
unit <- optional(SPC) %R% or("YO", "YR", "MO")
gender <- optional(SPC) %R% or("M", "F")
# Extract age, unit, gender
str_extract(narratives,pattern=age%R%unit%R%gender)
#set up variable
age_gender<-str_extract(narratives,pattern=age%R%unit%R%gender)
# Use str_extract with your age pattern to extract just the age from age_gender, then transform it to a number with as.numeric().
ls.str()
# Extract age and make numeric
as.numeric(str_extract(age_gender,age))
#Create genders by using str_remove() with your age %R% unit pattern to replace everything except the gender with "".
genders <- str_remove(age_gender,age%R%unit)
# Replace extra spaces
str_remove_all(genders, pattern = one_or_more(SPC))
# Numeric ages, from previous step
ages_numeric <- as.numeric(str_extract(age_gender, age))
# Get time_units by using str_extract() on age_gender with your unit pattern.
time_units <- str_extract(age_gender,unit)
# To know if the units are months or years we just need the first character after any spaces. Use str_extract() on time_units with the pattern WRD to get time_units_clean.
time_units_clean <- str_extract(time_units,WRD)
# Turn ages in months to years
ifelse(time_units_clean == "Y", ages_numeric, ages_numeric/12)


#CAPTURING PARTS OF A PATTERN
#In rebus, to denote a part of a regular expression you want to capture, you surround it with the function capture(). For example, a simple pattern to match an email address might be,

email <- one_or_more(WRD) %R% 
    "@" %R% one_or_more(WRD) %R% 
    DOT %R% one_or_more(WRD)
str_view("(wolverine@xmen.com)", pattern = email)  
#If you want to capture the part before the @, you simply wrap that part of the regular expression in capture():
    
    email <- capture(one_or_more(WRD)) %R% 
    "@" %R% one_or_more(WRD) %R% 
    DOT %R% one_or_more(WRD)
str_view("(wolverine@xmen.com)", pattern = email)  
#The part of the string that matches hasn't changed, but if we pull out the match with str_match() we get access to the captured piece:

str_match("(wolverine@xmen.com)", pattern =  email)  
#You'll explore this behavior with some more super hero email addresses.
hero_contacts<-c("(wolverine@xmen.com)","wonderwoman@justiceleague.org",
"thor@avengers.com")
# Capture parts between @ and . and after .
email <- capture(one_or_more(WRD)) %R% 
    "@" %R% capture(one_or_more(WRD)) %R% 
    DOT %R% capture(one_or_more(WRD))
# Check match hasn't changed
str_match(hero_contacts,pattern=email)
str_view(hero_contacts,pattern=email)
# Pattern from previous step
email <- capture(one_or_more(WRD)) %R% 
    "@" %R% capture(one_or_more(WRD)) %R% 
    DOT %R% capture(one_or_more(WRD))
# Use str_match() to pull out the match and capture the email pattern in hero_contacts, and print it to confirm all parts are captured.
email_parts <- str_match(hero_contacts,pattern=email)
email_parts
# Save host
host <- email_parts[,3]
host
# Add capture() to get digit parts
phone_pattern <- capture(three_digits) %R% zero_or_more(separator) %R% 
    capture(three_digits) %R% zero_or_more(separator) %R%
    capture(four_digits)
# Pull out the parts with str_match()
phone_numbers <- str_match(contact,phone_pattern)
# Put them back together
str_c("(",
      phone_numbers[,2],
      ") ",
      phone_numbers[,3],
      "-",
      phone_numbers[,4]
)


#EXTRACTING AGE AND GENDER AGAIN
# Add capture() to get age, unit and sex
pattern <- capture(optional(DGT) %R% DGT) %R%  
    optional(SPC) %R% capture(or("YO", "YR", "MO")) %R%
    optional(SPC) %R% capture(or("M", "F"))
# Pull out from narratives
str_match(narratives,pattern)
# Edit to capture just Y and M in units
pattern2 <- capture(optional(DGT) %R% DGT) %R%  
    optional(SPC) %R% capture(or("Y", "M")) %R% optional(or("O","R")) %R%
    optional(SPC) %R% capture(or("M", "F"))
# Check pattern
str_view(narratives,pattern2)
# Pull out pieces
str_match(narratives,pattern2)


#USING BACKREFERENCES IN PATTERNS
#Backreferences can be useful in matching because they allow you to find repeated patterns or words. Using a backreference requires two things: you need to capture() the part of the pattern you want to reference, and then you refer to it with REF1.

#Take a look at this pattern: capture(LOWER) %R% REF1. It matches and captures any lower case character, then is followed by the captured character: it detects repeated characters regardless of what character is repeated. To see it in action try this:
    
    str_view(c("hello", "sweet", "kitten"), 
             pattern = capture(LOWER) %R% REF1)
#If you capture more than one thing you can refer to them with REF2, REF3 etc. up to REF9, counting the captures from the left of the pattern.

#Let's practice with boy_names again. You might notice a change in this dataset. We've converted all names to lower case; you'll learn how to do that in the next chapter.
boy_names_backup<-boy_names
boy_names<-tolower(boy_names)
# Names with three repeated letters
repeated_three_times <- capture(LOWER) %R% REF1 %R% REF1
# Test it
str_view(boy_names, pattern = repeated_three_times, match = TRUE)
# See all the boy_names with a pair of letters repeated twice, e.g. abab, by capturing two lower case characters, then referring to the capture with REF1. Assign the pattern to pair_of_repeated.
pair_of_repeated <- capture(LOWER %R% LOWER) %R% REF1
# Test it
str_view(boy_names, pattern = pair_of_repeated, match = TRUE)
#See all the boy_names with a pair of letter followed by their reverse, e.g. abba, by capturing two lower case characters separately and combining with REF2 and REF1. Assign the pattern to pair_that_reverses.
pair_that_reverses <- capture(WRD)%R%capture(WRD)%R%REF2%R%REF1
# Test it
str_view(boy_names, pattern = pair_that_reverses, match = TRUE)
# Four letter palindrome names
four_letter_palindrome <- exactly(capture(WRD)%R%capture(WRD)%R%REF2%R%REF1)
# Test it
str_view(boy_names, pattern = four_letter_palindrome, match = TRUE)


#REPLACING WITH REGULAR EXPRESSIONS
#Now, you've mastered matching with backreferences, you'll build up to replacing with backreferences, but first let's review str_replace() now that you've got regular expressions under your belt.
#Remember str_replace() takes three arguments, string a vector of strings to do the replacements in, pattern that identifies the parts of strings to replace and replacement the thing to use as a replacement.
#replacement can be a vector, the same length as string, each element specifies the replacement to be used in each string. Let's practice by anonymizing some of the contact objects you've seen so far.
# Replace digits with "X"
str_replace(contact, DGT, "X")
# Replace all digits with "X"
str_replace_all(contact, DGT, "X")
# Replace all digits with different symbol
str_replace_all(contact, DGT, c("X", ".", "*", "_"))


#REPLACING WITH BACKREFERENCES
#The replacement argument to str_replace() can also include backreferences. This works just like specifying patterns with backreferences, except the capture happens in the pattern argument, and the backreference is used in the replacement argument.
x <- c("hello", "sweet", "kitten")
str_replace(x, capture(ANY_CHAR), str_c(REF1, REF1))
#capture(ANY_CHAR) will match the first character no matter what it is. Then the replacement str_c(REF1, REF1) combines the captured character with itself, in effect doubling the first letter of each string.
#You are going to use this to create some alternative, more lively accident narratives.
#The strategy you'll take is to match words ending in "ING" then replace them with an adverb followed by the original word.
adverbs<- c("ABNORMALLY",     "ABSENTMINDEDLY", "ACCIDENTALLY",   "ACIDLY"  ,      "ACTUALLY" ,      "ADVENTUROUSLY" , "AFTERWARDS"  ,   "ALMOST"     ,   "ALWAYS" ,        "ANGRILY"   ,     "ANNUALLY" ,      "ANXIOUSLY"   ,  "ARROGANTLY"  ,   "AWKWARDLY"  ,    "BADLY"     ,     "BASHFULLY"   ,  "BEAUTIFULLY" ,   "BITTERLY"   ,    "BLEAKLY"  ,      "BLINDLY"    ,   "BLISSFULLY" ,    "BOASTFULLY"  ,   "BOLDLY"   ,      "BRAVELY"   ,    "BRIEFLY")
# Build pattern to match words ending in "ING"
pattern <- one_or_more(WRD) %R% "ING" 
str_view(narratives, pattern)
#Test out the replacement by using str_replace() with your pattern (don't forget to capture() it!) and a replacement str_c("CARELESSLY", REF1, sep = " ").
str_replace(narratives, capture(pattern), str_c("CARELESSLY", REF1, sep = " "))    
# Build a vector with one adverb for each narrative by sampling 10 elements from adverbs.
adverbs_10<-sample(adverbs,size=10,replace=FALSE)
# Replace "***ing" with "adverb ***ly" by using str_c(adverbs_10, REF1, sep = " ")
str_replace(narratives, capture(pattern), str_c(adverbs_10, REF1, sep = " "))



#MATCHING A SPECIFIC CODE POINT OR CODE GROUPS
library(stringi)
#Things can get tricky when some characters can be specified two ways, for example è, an e with a grave accent, can be specified either with the single code point \u00e8 or the combination of a \u0065 and a combining grave accent \u0300. They look the same:
    x <- c("\u00e8", "\u0065\u0300")
writeLines(x)
#But, specifying the single code point only matches that version:
    str_view(x, "\u00e8")
#The stringi package that stringr is built on contains functions for converting between the two forms. stri_trans_nfc() composes characters with combining accents into a single character. stri_trans_nfd() decomposes character with accents into separate letter and accent characters. You can see how the characters differ by looking at the hexadecimal codes.
as.hexmode(utf8ToInt(stri_trans_nfd("\u00e8")))
as.hexmode(utf8ToInt(stri_trans_nfc("\u0065\u0300")))
#In Unicode, an accent is known as a diacritic Unicode Property, and you can match it using the rebus value UP_DIACRITIC.
#Vietnamese makes heavy use of diacritics to denote the tones in its words. In this exercise, you'll manipulate the diacritics in the names of Vietnamese rulers.
# Names with builtin accents
(tay_son_builtin <- c(
    "Nguy\u1ec5n Nh\u1ea1c", 
    "Nguy\u1ec5n Hu\u1ec7",
    "Nguy\u1ec5n Quang To\u1ea3n"
))
# Call stri_trans_nfd() to decompose the letters with accents into separate letter and accent characters, and assign the result to tay_son_separate.
tay_son_separate <- stri_trans_nfd(tay_son_builtin)
# Verify that the string prints the same
tay_son_separate
# View all the accents by calling str_view_all() and matching UP_DIACRITIC. The match is shown after the letter that the diacritic belongs to.
str_view_all(tay_son_separate, UP_DIACRITIC)



#MATCHING A SINGLE GRAPHEME
#A related problem is matching a single character. You've used ANY_CHAR to do this up until now, but it will only match a character represented by a single code point. Take these three names:
x <- c("Adele", "Ad\u00e8le", "Ad\u0065\u0300le")
writeLines(x)
#They look the similar, but this regular expression only matches two of them:
    str_view(x, "Ad" %R% ANY_CHAR %R% "le")
#because in the third name è is represented by two code points. The Unicode standard has a concept of a grapheme that represents a display character, but may be composed of many code points. To match any grapheme you can use GRAPHEME.
str_view(x, "Ad" %R% GRAPHEME %R% "le")
#Names of rulers from the Vietnamese Tây Son dynasty, with diacritics given as separate graphemes, is pre-defined as tay_son_separate.
# tay_son_separate has been pre-defined
tay_son_separate
# View all the characters in tay_son_separate
str_view_all(tay_son_separate,ANY_CHAR)
# View all the graphemes in tay_son_separate
str_view_all(tay_son_separate,GRAPHEME)
# Combine the diacritics with their letters
tay_son_builtin <- stri_trans_nfc(tay_son_separate)
tay_son_builtin
# View all the graphemes in tay_son_builtin
str_view_all(tay_son_builtin,GRAPHEME)




#GETTING THE PLAY INTO R
#We've already downloaded the play and put the text file in your workspace. Your first step is to read the play into R using stri_read_lines().
#You should take a look at the original text file: importance-of-being-earnest.txt
#You'll see there is some foreword and afterword text that Project Gutenberg has added. You'll want to remove that, and then split the play into the introduction (the list of characters, scenes, etc.) and the main body.

#download the file from: http://s3.amazonaws.com/assets.datacamp.com/production/course_2922/datasets/importance-of-being-earnest.txt
earnest<-c(
"The Project Gutenberg eBook, The Importance of Being Earnest, by Oscar",
"Wilde"                                                                 ,
""                                                                      ,
""                                                                      ,
"This eBook is for the use of anyone anywhere at no cost and with"      ,
"almost no restrictions whatsoever.  You may copy it, give it away or"  ,
"re-use it under the terms of the Project Gutenberg License included"   ,
"with this eBook or online at www.gutenberg.org"                        ,
""                                                                      ,
""                                                                      ,
""                                                                      ,
""                                                                      ,
""                                                                      ,
"Title: The Importance of Being Earnest"                                ,
"       A Trivial Comedy for Serious People",
"***START OF THE PROJECT GUTENBERG EBOOK THE IMPORTANCE OF BEING EARNEST***",
"Transcribed from the 1915 Methuen & Co. Ltd. edition by David Price,",
"Transcribed from the 1915 Methuen & Co. Ltd. edition by David Price,",
"Jack.  Yes, I know it is.  But supposing it was something else?  Do you" ,
"mean to say you couldn't love me then?"                                  ,
""                                                                        ,
"Gwendolen.  [Glibly.]  Ah! that is clearly a metaphysical speculation,"  ,
"and like most metaphysical speculations has very little reference at all",
"to the actual facts of real life, as we know them."                      ,
""                                                                        ,
"Jack.  Personally, darling, to speak quite candidly, I don't much care",
"***END OF THE PROJECT GUTENBERG EBOOK THE IMPORTANCE OF BEING EARNEST***"
)
# Read play in using stri_read_lines()
earnest <- stri_read_lines(earnest_file)
#Find the lines that end the foreword and start of afterword by detecting the patterns "START OF THE PROJECT" and "END OF THE PROJECT".
start <- str_which(earnest, "START OF THE PROJECT")
end <- str_which(earnest, "END OF THE PROJECT")
# Get rid of gutenberg intro text: Use the start and end positions to subset the play to the lines between (start + 1) and (end - 1).
earnest_sub  <- earnest[(start+1):(end-1)]
# Detect first act 
lines_start <- str_which(earnest_sub, "FIRST ACT")
# Build an index that captures the intro_line_index with 1:(lines_start - 1).
intro_line_index <- 1:(lines_start-1)
# Split play into intro and play
intro_text <- earnest_sub[intro_line_index]
play_text <- earnest_sub[-intro_line_index]
# Take a look at the first 20 lines
writeLines(play_text)














