library(GeoexperimentsResearch)

#sample data
data(salesandcost)
head(salesandcost)

# convert this data frame into a GeoTimeseries object 
# and the integrity of the time series is automatically checked
# We need to specify which columns are to be treated as metrics
obj.gts <- GeoTimeseries(salesandcost, metrics=c("sales", "cost"))

# The resulting object inherits from
# data.frame, with the same columns, augmented with some extra columns:
head(obj.gts)

# The ‘date’ column must be in either ‘Date’, factor, or character format and is always coerced
# to Date. If the date format differs from ‘yyyy-mm-dd’, it is necessary to specify it as argument
# ‘date.format’.

# weekday column denotes the day of the week (1=Monday, 7=Sunday);
# weeknum indicates the number of the week within a year;
# weekindex indicates a unique week number.


########
# Exploratory data analysis

# investigate the distribution of the metrics across weeks, we can use the aggregate method
aggregate(obj.gts, by='.weekindex')

plot(obj.gts, legend=TRUE)


#############
# Experiment Periods

# e specify the start of the Pretest period, the start of the test period, and the end of the experiment. If
# there is a Cooldown period after the actual market intervention, 
# it must be included as a separate period
# (four dates in total).

obj.per <- ExperimentPeriods(c("2015-01-05", "2015-02-16", "2015-03-15"))
obj.per

# To introduce a cooldown period, we would specify one more date

##########
# Geo Assignment

# sample
data(geoassignment)
head(geoassignment)

# From this data frame we create a GeoAssignment object and automatically verify its integrity
obj.ga <- GeoAssignment(geoassignment)
head(obj.ga)


##########
# Combining all information about the experiment into one object

# class GeoExperimentData combines these three pieces of 
# information (geo time series, periods, geo
# assignment) into one object:
obj <- GeoExperimentData(obj.gts,
                         periods=obj.per,
                         geo.assignment=obj.ga)
head(obj)


# The column period contains the indicator for the experiment periods: 
# 0 = Pretest, 1 = test (Intervention).
# ‘NA’ marks a date that is outside of the designated experiment periods.
# The column geo.group contains the geo group ID for each of the geos.
# The column assignment is not used in this version of the R package. 
# It is set to NA by default. It can be
# ignored.


# Exploratory data analysis
# check how the revenue and cost metrics are distributed across periods and groups, 
# we make use of the
# aggregate method again:
  aggregate(obj, by=c('period', 'geo.group'))

  
  ##############
  # Geo-Based Regression (GBR) Analysis
  
  
# To perform a GBR (geo-based regression) analysis, 
#  apply method DoGBRROASAnalysis, specifying which
#  of the metrics is the response and which represents the cost, 
#  along with the experiment periods and group
#  numbers.
  
  result <- DoGBRROASAnalysis(obj, response='sales', cost='cost',
                              pretest.period=0,
                              intervention.period=1,
                              cooldown.period=NULL,
                              control.group=1,
                              treatment.group=2)
  result

  # Note that in this particular case, there is no cooldown.period, 
  # hence it is set to NULL. If there was one, we
  # would specify the period number (for example, cooldown.period=2).  
  
  # The resulting object (a GBRROASAnalysisFit object) contains the model fit: 
  # when printed, it shows its
  # summary, which defaults to 90 percent credible intervals. 
  # To recalculate the interval with a different credibility
  # level, we can specify this in the function call:
    
    summary(result, level=0.95, interval.type="two-sided")

# To obtain the posterior probability that the true iROAS is larger than some threshold, 
# say 3.0, we use the
# summary method as follows:
      summary(result, threshold=3.0)    
      
# The default threshold is 0.      

    
#####################3
# Time-Based Regression (TBR) ROAS Analysis
# The GeoExperimentData object can also be used for performing a TBR analysis,
     # applying method
    #  DoTBRROASAnalysis, specifying which of the metrics is the response 
    #  and which represents the cost, along
    #  with the experiment period and group numbers. 
    #  The model ID is also required; currently the only available
    #  model is ‘tbr1’
      obj.tbr.roas <- DoTBRROASAnalysis(obj, response='sales', cost='cost',
                                        model='tbr1',
                                        pretest.period=0,
                                        intervention.period=1,
                                        cooldown.period=NULL,
                                        control.group=1,
                                        treatment.group=2)
      obj.tbr.roas      

      #The resulting object (a TBRROASAnalysisFit object) contains the model fit: 
      # when printed, it shows its
      # summary, which defaults to 90 percent one-sided credible intervals. 
      # Similarly to what we did with a
      # GBRROASAnalysisFit object we can recalculate the credible interval, 
      # and the probability of exceeding a
      # given threshold like so:
      
      summary(obj.tbr.roas, level=0.95, interval.type="two-sided")
      summary(obj.tbr.roas, threshold=3.0)      

    # The plot method shows the evolution of the iROAS estimate across the Test period:
      plot(obj.tbr.roas)

      ###################
      # Time-Based Regression (TBR) Causal Effect Analysis
      
# Unlike the TBR ROAS Analysis, which estimates the ratio of the incremental response 
      # and incremental cost,
      # the TBR Causal Effect Analysis applies only to one single variable, such as revenue.      
      
      obj.tbr <- DoTBRAnalysis(obj, response='sales',
                               model='tbr1',
                               pretest.period=0,
                               intervention.period=1,
                               cooldown.period=NULL,
                               control.group=1,
                               treatment.group=2)

      # The resulting object (a TBRAnalysisFitTbr1 object) contains the model fit 
      # for each time point, which can be
      # seen when printed. To show the summary of the effect, we use the summary method
      
      summary(obj.tbr)
# The plot method illustrates the results of the analysis.
      
      plot(obj.tbr)

      
##################################
      # Pre-analysis
      
      
# A randomized geo assignment

      # Randomized geo assignments can be done using ‘GeoStrata’ objects. 
      # This object includes a mapping from
      # each geo to a stratum (or block), so a stratified randomization can be performed. 
      # This can be generated
      # automatically using the ExtractGeoStrata function:
      obj.geo.strata <- ExtractGeoStrata(obj.gts, volume="sales", n.groups=2)
      head(obj.geo.strata)

      # The argument ‘volume’ specifies the name of the metric that is used for stratification: 
      # the geos are sorted by
      # their volume and divided into strata of 2 each.      
      
      # To generate a randomized geo assignment, we use the ‘Randomize’ method:
      obj.geo.assignment <- Randomize(obj.geo.strata)
      head(obj.geo.assignment)

      # Predicting the precision
      
      # We pass this object to the method DoGBRPreanalysis along with the GeoTimeseries, 
      # the length of the
      # Pretest, Intervention, and Cooldown periods, and specify a metric:
      
      obj.pre <- DoROASPreanalysis(obj.gts, response="sales",
                                   geos=obj.geo.assignment,
                                   prop.to="sales",
                                   period.lengths=c(42, 21, 7))
# The resulting object ‘obj.pre’ is of class ROASPreanalysisFit, 
      # which only contains the raw simulated numbers.
      # To compute the required spend for precision +/- 1.0, we call the summary method:      
      
      results <- summary(obj.pre,
                         level=0.90,
                         type="one-sided",
                         precision=1.0)
      print(results)

      # For convenience, applying the ‘print’ method on the “GBRPreanalysisFit” 
      # prints the default summary
      # (one-sided confidence interval at level 0.90, precision 1.0):      
      print(obj.pre)

      # The function can be also used to predict the precision 
      # given a (total) spend change over the test period      
      results2 <- summary(obj.pre,
                          level=0.90,
                          type="one-sided",
                          cost=10000)
      print(results2)
      
