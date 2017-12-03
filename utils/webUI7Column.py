# Author: Collin Mitchell
# Date: 2017/04/27
# Updated: 2017/07/29
# Purpose: To create a schema for R to parse UI logs correctly.

# used to check all files in target
import glob

# open connection to output file.
with open("output.csv", "a+") as out:
	# collect list of log files.
	for fileTarget in glob.glob('*.log*'):
		# open file to parse and append.
		with open(fileTarget) as f:
			for line in f:
				# prepare values
				values = line.split(' ')
				DATEINDEX, INFOINDEX = 2, 6
				dateValues, timezone, middleValues, infoValue, dataValues = "NA", "NA", "NA", "NA", "NA"

				# if issues
				if len( values ) < INFOINDEX+2:
					dataValues = ' '.join(values[:])

				# If no issues
				elif( "INFO" == values[INFOINDEX] or "ERROR" == values[INFOINDEX] or "DEBUG" == values[INFOINDEX]):
					dateValues   = ' '.join( values[ 0:DATEINDEX ])
					timezone     =           values[ DATEINDEX ]
					middleValues = ' '.join( values[ DATEINDEX+1:INFOINDEX ])
					infoValue    =           values[ INFOINDEX ]
					dataValues   = ' '.join( values[ INFOINDEX+1: ] )

				# if +1 column deviation
				elif("INFO" == values[INFOINDEX+1] or "ERROR" == values[INFOINDEX+1] or "DEBUG" == values[INFOINDEX+1]):
					dateValues   = ' '.join( values[ 0:DATEINDEX ])
					timezone     =           values[ DATEINDEX ]
					middleValues = ' '.join( values[ DATEINDEX+1:INFOINDEX+1 ])
					infoValue    =           values[ INFOINDEX+1 ]
					dataValues   = ' '.join( values[ INFOINDEX+2: ] )

				else:
					dataValues = ' '.join(values[:])

				# output data
				out.write( fileTarget + '~' + dateValues +  '~' +  timezone + '~' + middleValues + '~' + infoValue + '~' + dataValues + "\n")
