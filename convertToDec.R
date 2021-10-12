convertToDec <- function(input, base)
{
  #There are several goals with this function: to alert the user if a conversion has the 'wrong' base (e.g. 'FFF' in base 14,
  #even though there is no 'F' in base 14); to catch a user that inputs characters which aren't alphanumeric; and to
  #not be case-sensitive with letters
  dec <- 0
  digit <- 0
  len <- nchar(input)
  for(i in 1:len)
  {
    char <- substr(input, i, i)
    if(utf8ToInt(char)>=48 & utf8ToInt(char)<=57) #this is for numbers 0-9
    {
      digit <- as.integer(char)
    }
    else if(utf8ToInt(char)>=97 & utf8ToInt(char)<=122) #this is for 'a'-'z'
    {
      digit <- utf8ToInt(char)-87 #the reason to subtract 87 is so that 'a' means 10, 'b' means 11, etc.
    }
    else if(utf8ToInt(char)>=65 & utf8ToInt(char)<=90) #this is for 'A'-'Z'
    {
      digit <- utf8ToInt(char)-55 #same logic as with lowercase letters
    }
    else
    {
      print('All characters must be either numeric or alphabetical. Alphabetical characters are not case-sensitive')
      i = len+1 #this terminates the for loop after the current iteration
    }
    if(digit>=base) #and here is where we catch a user who asks for e.g. 'BAR' in base 13 (base 28 is where 'R' starts to appear)
    {
      print(paste('You need a base of at least ', digit+1, ' for this to be a possible value'))
      digit <- 0
      i = len+1
    }
    dec <- dec + base**(len-i)*digit
  }
  dec
}