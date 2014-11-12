import math

data = [.81, .83, .79, .75, .8]
n = len(data)

smx = sum(data) # sum of data
slx  = 0  # sum of log of data
sl1mx = 0 # sum of log of (1-data)

# Compute slx
for d in data:
  slx += math.log(d)
  sl1mx += math.log(1-d)


def la(a,b):
  return (4.0*math.log(a)-a/20.0+n*(math.lgamma(a+b)-math.lgamma(a))+(a-1)*slx)


def lb(a,b):
  return (7.0*math.log(b)-b/30.0+n*(math.lgamma(a+b)-math.lgamma(b))+(b-1)*sl1mx)


def mig(csa=50,csb=20,B=1e6):


#http://stackoverflow.com/questions/6125692/print-r-correctly-in-console
for i in range(101):
    print(str(i) + "% \r")

