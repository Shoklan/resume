# Author: Collin Mitchell
# Date: 2017/03/22
# Purpose: To create a schema for R to parse General Typed Format Logs correctly.

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
				DATEINDEX, DATAINDEX = 2, 5

				# what is the condition that would allow
				# me to combine the if and else block?
				if len( values ) <= 4:
					dateValues   = '~'.join(['NA', 'NA'])
					middleValues = '~'.join(['NA', 'NA'])
					dataValues   = ' '.join( values[:])
				# if no issues.
				elif( 'INFO' == values[4] or "ERROR" == values[4] or "WARN" == values[4] or "FATAL" == values[4]):
					dateValues   = ' '.join( values[ 0:DATEINDEX ])
					middleValues = '~'.join( values[ DATEINDEX:DATAINDEX ])
					dataValues   = ' '.join( values[ DATAINDEX: ])
				# distorted data line
				else:
					dateValues   = '~'.join(['NA', 'NA'])
					middleValues = '~'.join(['NA', 'NA'])
					dataValues   = ' '.join( values[:])

				# output data to csv
				out.write( fileTarget + '~' + dateValues + '~' + middleValues + '~' + dataValues + "\n")


