import unicodecsv as csv
import os
import xlrd
from xlrd import open_workbook
from xlutils.copy import copy
import xlwt

# -*- coding: utf8 -*-


def main():
	parent_path = "C:\Users\User\Desktop\econometrics_final/market data/"
	input_path = parent_path 
	output_path = "C:\Users\User\Desktop\econometrics_final/ABT data/"
	file_list = os.listdir(input_path)
	final_dict = {}
	
	after_Qdict = {}
	after_Pdict = {}
	before_Qdict= {}
	before_Pdict= {}	
	after_name= []
	for i in range(0, len(file_list)):
		print file_list[i]
		wb = open_workbook(parent_path + file_list[i])
		values = []
		s= wb.sheets()[0]
		#for s in wb.sheets():
		#print 'Sheet:',s.name
		if(i%2==0):
			after_type = s.nrows 
			after_name= []
		elif(i%2==1):
			before_type= s.nrows

			if (before_type - after_type !=0):
				print "problem"
			

		col_names = s.row(0)
		
		
		for row in range(1, s.nrows):
			
			for col in range(s.ncols):
				#col_value = []
				name = "Hi"
				if(col==0):
					#print s.cell(row,col).value
					name= s.cell(row,col).value
					
					if(i%2==0):						
						after_name.append(name)				
					
					values.append(name)
				else:
						
					value  = (s.cell(row,col).value)
					#col_value.append(int(value))
					#print(col_value)
					values.append(value)
		#print values
		#print after_name
		
		for k in range(s.nrows-1):
			if(i%2==0):				
				#print k*5
				
				mean_price = float(values[k*5+2]+values[k*5+4])*0.2+ float(values[k*5+3])*0.6
				
				after_Qdict[after_name[k]] = values[k*5+1]
				after_Pdict[values[k*5]] = mean_price
			elif(i%2==1):
				#mean_price = (float(values[2])+float(values[4]))*0.2+ float(values[3])*0.6

				mean_price = float(values[k*5+2]+values[k*5+4])*0.2+ float(values[k*5+3])*0.6	
				before_Qdict[values[k*5]] = values[k*5+1]
				before_Pdict[values[k*5]] = mean_price
		
		if(i%2==1):
			filename= file_list[i]
			index = filename.find("_")
			typhon_ID = filename[:index]
			final_dict[typhon_ID]={}
			#final_dict[typhon_ID].setdefault(str, [])
			#0:Q下降 1:P下降
			
			#print "before"
			#for i in before_Qdict.keys():
			#	print i
			#	print before_Qdict[i]
			
			#print "after"
			#for m in after_Qdict.keys():
			#	print m
			#	print after_Qdict[m]
				
			
			final_dict[typhon_ID]["A"] = [None]*6
			final_dict[typhon_ID]["B"] = [None]*6
			final_dict[typhon_ID]["Target"] = [None]*6
			
			
			final_dict[typhon_ID]["A"][0] = ((after_Qdict[after_name[0]]-before_Qdict[values[0]] ) / before_Qdict[values[0]])
			final_dict[typhon_ID]["A"][1] = before_Qdict[values[0]]
			final_dict[typhon_ID]["A"][2] = after_Qdict[after_name[0]]
			
			final_dict[typhon_ID]["A"][3] = (after_Pdict[after_name[0]]- before_Pdict[values[0]]) / before_Pdict[values[0]]
			final_dict[typhon_ID]["A"][4] = before_Pdict[values[0]]
			final_dict[typhon_ID]["A"][5] = after_Pdict[after_name[0]]
			 
 
			

			
			if( (u'大白菜' not in values[5*1]) & (u'包心白菜' not in values[5*1])):
			
				temp_cab = final_dict[typhon_ID]["A"][0] *  before_Qdict[values[0]] + (after_Qdict[after_name[1]] - before_Qdict[values[1*5]] )
				final_dict[typhon_ID]["A"][0] = temp_cab / (before_Qdict[values[1*5]] + before_Qdict[values[0]])
				
				temp_cabP = final_dict[typhon_ID]["A"][3] *  before_Pdict[values[0]]* before_Qdict[values[0]] + (after_Pdict[after_name[1]] - before_Pdict[values[1*5]] )* before_Qdict[values[1*5]]
				temp_avg_cabP = (before_Pdict[values[0]]* before_Qdict[values[0]] +  before_Pdict[values[1*5]]* before_Qdict[values[1*5]])
				final_dict[typhon_ID]["A"][3] = temp_cabP / temp_avg_cabP
				
				final_dict[typhon_ID]["B"][0]=(after_Qdict[after_name[2]]- before_Qdict[values[2*5]]) / before_Qdict[values[2*5]]
				final_dict[typhon_ID]["B"][3]=(after_Pdict[after_name[2]]- before_Pdict[values[2*5]]) / before_Pdict[values[2*5]]
				target_start = 3
			else:
				final_dict[typhon_ID]["B"][0] = (after_Qdict[after_name[1]]- before_Qdict[values[1*5]]) / before_Qdict[values[1*5]]
				final_dict[typhon_ID]["B"][3] = (after_Pdict[after_name[1]]- before_Pdict[values[1*5]]) / before_Pdict[values[1*5]]
				target_start = 2		
			
			
			
			final_dict[typhon_ID]["B"][1] = before_Qdict[values[1*5]]
			final_dict[typhon_ID]["B"][2] = after_Qdict[after_name[1]]
			final_dict[typhon_ID]["B"][4] = before_Pdict[values[1*5]]
			final_dict[typhon_ID]["B"][5] = after_Pdict[after_name[1]]
			
			
			target_Q_change=0
			target_Q_before=0
			target_Q_after=0
			
			target_P_change=0
			target_P_before=0
			target_P_after=0
			
			for target_index in range(target_start, s.nrows-1) :
				#print target_index
				target_Q_change += ( after_Qdict[after_name[target_index]] - before_Qdict[values[target_index*5]])
				target_Q_before += before_Qdict[values[target_index*5]]
				target_Q_after	+= after_Qdict[after_name[target_index]]
				
				target_P_change += ( after_Pdict[after_name[target_index]] - before_Pdict[values[target_index*5]])*before_Qdict[values[target_index*5]]
				target_P_before += before_Pdict[values[target_index*5]]*before_Qdict[values[target_index*5]]
				target_P_after	+= after_Pdict[after_name[target_index]]*after_Qdict[after_name[target_index]]
		
		
			final_dict[typhon_ID]["Target"][0] = (target_Q_change / target_Q_before)
			final_dict[typhon_ID]["Target"][3] = target_P_change / target_P_before
			
			final_dict[typhon_ID]["Target"][1] = target_Q_before
			final_dict[typhon_ID]["Target"][2] = target_Q_after
			final_dict[typhon_ID]["Target"][4] = target_P_before / target_Q_before
			final_dict[typhon_ID]["Target"][5] = target_P_after / target_Q_after
			
	

			
			after_Qdict = {}
			after_Pdict = {}
			before_Qdict = {}
			before_Pdict = {}

	#for l in final_dict.keys() :
	#	print u'甘藍 ', final_dict[l]["A"]
	#	print u'大白菜 ', final_dict[l]["B"]
	#	print u'短期葉菜類 ', final_dict[l]["Target"]
	
	output_file = open(output_path+"historical_market_data.csv", "w")
	w = csv.writer(output_file,  encoding='utf-8')
	headers = [u'颱風編號', u'甘藍菜數量變化', u'甘藍菜颱風前批發量', u'甘藍菜颱風後批發量',u'甘藍菜價變化', u'甘藍菜颱風前菜價', u'甘藍菜颱風後菜價', u'大白菜數量變化', u'大白菜颱風前批發量', u'大白菜颱風後批發量',u'大白菜價變化', u'大白菜颱風前菜價', u'大白菜颱風後菜價',u'短期葉菜數量變化', u'短期葉菜颱風前批發量', u'短期葉菜颱風後批發量',  u'短期葉菜菜價變化', u'短期葉菜颱風前菜價', u'短期葉菜颱風後菜價']
	
	w.writerows([headers])
	for writeing in final_dict:
		write_temp = [writeing] + final_dict[writeing]["A"]+ final_dict[writeing]["B"]+ final_dict[writeing]["Target"]
		w.writerows([write_temp])
	
	

			
main()