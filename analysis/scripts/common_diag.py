#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os
import numpy as np
import struct

def read_diag_sat( file_name ) :
    
    #Read a radiance diag file and return a dictionary with the data.
    
    #Initialize the dictionary.
    diag_content = dict()


    if  os.path.exists( file_name ) :
        f=open( file_name ,'rb')
        
        #Read file header =====================================================
        rh=np.fromfile(f,dtype='>i4',count=1)[0] #Seq acces read record header        
        isis     = np.fromfile( f , dtype='20c' , count = 1).tostring().decode('utf-8')
        dplat    = np.fromfile( f , dtype='10c' , count = 1).tostring().decode('utf-8')
        obstype  = np.fromfile( f , dtype='10c' , count = 1).tostring().decode('utf-8')
        
        jiter    = np.fromfile( f , dtype='>i4' , count = 1)[0]
        nchanl   = np.fromfile( f , dtype='>i4' , count = 1)[0] 
        npred    = np.fromfile( f , dtype='>i4' , count = 1)[0]
        idate    = np.fromfile( f , dtype='>i4' , count = 1)[0]     
        ireal    = np.fromfile( f , dtype='>i4' , count = 1)[0]        
        ipchan   = np.fromfile( f , dtype='>i4' , count = 1)[0] 
        iextra   = np.fromfile( f , dtype='>i4' , count = 1)[0] 
        jextra   = np.fromfile( f , dtype='>i4' , count = 1)[0] 
        
        #Not shure why I need to add this but the record says that there where missing more information here.
        np.fromfile( f , dtype='>i4' , count = 6 ) 
           
        rh=np.fromfile(f,dtype='>i4',count=1)[0] #Seq acces read record header

        # print('Satellite name     ',dplat)
        # print('Sensor             ',obstype)
        # print('Number of channels ',nchanl)
        
        freq4    =np.zeros( nchanl )
        pol4     =np.zeros( nchanl )
        wave4    =np.zeros( nchanl )
        varch4   =np.zeros( nchanl )
        tlap4    =np.zeros( nchanl )        
        iuse_rad =np.zeros( nchanl ) 
        nuchan   =np.zeros( nchanl )
        ich      =np.zeros( nchanl )
        
        for ichan in range( nchanl ) :
           rh=np.fromfile(f,dtype='>i4',count=1)[0] #Seq acces read record header
           
           freq4[ichan]      = np.fromfile( f , dtype='>f4' , count = 1 )[0]
           pol4[ichan]       = np.fromfile( f , dtype='>f4' , count = 1 )[0]
           wave4[ichan]      = np.fromfile( f , dtype='>f4' , count = 1 )[0]
           varch4[ichan]     = np.fromfile( f , dtype='>f4' , count = 1 )[0]
           tlap4[ichan]      = np.fromfile( f , dtype='>f4' , count = 1 )[0]
           iuse_rad[ichan]   = np.fromfile( f , dtype='>i4'      , count = 1 )[0]
           nuchan[ichan]     = np.fromfile( f , dtype='>i4'      , count = 1 )[0]
           ich[ichan]        = np.fromfile( f , dtype='>i4'      , count = 1 )[0]
           
           rh=np.fromfile(f,dtype='>i4',count=1)[0] #Seq acces read record header
          
        
        diag_content['freq'] = np.copy( freq4 )
        diag_content['iuse_rad'] = np.copy( iuse_rad )
        diag_content['nuchan'] = np.copy( nuchan )
        diag_content['tlap'] = np.copy( tlap4 )
        diag_content['pol'] = np.copy( pol4 )
        diag_content['wave'] = np.copy( wave4 )
        diag_content['sat'] = np.copy( dplat )
        diag_content['sensor'] = np.copy( obstype )
        diag_content['nchanl'] = np.copy( nchanl )
        diag_content['date'] = np.copy( idate )
        diag_content['npred'] = np.copy( npred )
               
        #End of file header ===================================================
        
        #Get the number of records ============================================
        
        diagbufchan=np.zeros((ipchan+npred+3,nchanl))
        diagbuf=np.zeros(ireal)
        diagbufex=np.zeros(1,nchanl)
        
        nrecord = 0
        cont = True
        while cont :
           try :
              rh=np.fromfile(f,dtype='>i4',count=1)[0] #Seq acces read record header
              np.fromfile(f,dtype='>f4',count=ireal+nchanl*(ipchan+npred+3)+nchanl)
              rh=np.fromfile(f,dtype='>i4',count=1)[0] #Seq acces read record header
              nrecord = nrecord + 1
           except :
              #print('File size is ',nrecord)
              cont = False
        
        f.seek(0)  #Rewind the file. 
        #Read file header =====================================================
        np.fromfile( f , dtype='>i4' , count = 26 ) 
        for ichan in range( nchanl ) :
           np.fromfile( f , dtype='>i4' , count = 10 )            
          
        #Read the data ========================================================
        
        diag_content['lat'] = np.zeros( nrecord )
        diag_content['lon'] = np.zeros( nrecord )
        diag_content['prs'] = np.zeros( nrecord )
        diag_content['rdhr'] = np.zeros( nrecord )
        diag_content['rzen'] = np.zeros( nrecord )
        diag_content['razi'] = np.zeros( nrecord )
        diag_content['rlnd'] = np.zeros( nrecord )
        diag_content['rice'] = np.zeros( nrecord )
        diag_content['rsnw'] = np.zeros( nrecord )
        diag_content['rcld'] = np.zeros( nrecord )
        diag_content['rcldp'] = np.zeros( nrecord )  #TODO: detct microwave sensors to attribute this to the right variable.
  
        
        diag_content['predictors']= np.zeros(( npred , nrecord , nchanl ))
        diag_content['tb_obs'] = np.zeros(( nrecord , nchanl ))
        diag_content['tbc']    = np.zeros(( nrecord , nchanl ))
        diag_content['tbcnob'] = np.zeros(( nrecord , nchanl ))       
        diag_content['errinv']    = np.zeros(( nrecord , nchanl ))
        diag_content['qc']    = np.zeros(( nrecord , nchanl ))
        diag_content['emis']    = np.zeros(( nrecord , nchanl ))
        diag_content['tlapchn']    = np.zeros(( nrecord , nchanl ))
        diag_content['peakwt']    = np.zeros(( nrecord , nchanl ))
        
        for irec in range(nrecord) :
           #Read the data
           rh=np.fromfile(f,dtype='>i4',count=1)[0] #Seq acces read record header
           meta_data_buf = np.fromfile(f,dtype='>f4',count=ireal)
           data_buf = np.fromfile(f,dtype='>f4',count=nchanl*(ipchan+npred+3) ).reshape(nchanl,(ipchan+npred+3)).T
           ext_data_buf  = np.fromfile(f,dtype='>f4',count=nchanl)
           rh=np.fromfile(f,dtype='>i4',count=1)[0] #Seq acces read record header
              
           #Distribute the data into a more meaningful way.
           diag_content['lat'][irec] = meta_data_buf[0]
           diag_content['lon'][irec] = meta_data_buf[1]              
           diag_content['prs'][irec] = meta_data_buf[2]
           diag_content['rdhr'][irec] = meta_data_buf[3]
           diag_content['rzen'][irec] = meta_data_buf[5]
           diag_content['razi'][irec] = meta_data_buf[6]
           diag_content['rlnd'][irec] = meta_data_buf[11]
           diag_content['rice'][irec] = meta_data_buf[12]
           diag_content['rsnw'][irec] = meta_data_buf[13]
           diag_content['rcld'][irec] = meta_data_buf[24]
           diag_content['rcldp'][irec] = meta_data_buf[25]
              
           diag_content['tb_obs'][irec,:]  = data_buf[0,:] 
           diag_content['tbc'][irec,:]     = data_buf[1,:]            
           diag_content['tbcnob'][irec,:]  = data_buf[2,:]              
           diag_content['errinv'][irec,:]  = data_buf[3,:]              
           diag_content['qc'][irec,:]      = data_buf[4,:]
           diag_content['emis'][irec,:]    = data_buf[5,:]
           diag_content['tlapchn'][irec,:] = data_buf[6,:]
           diag_content['peakwt'][irec,:]  = ext_data_buf[:]

           diag_content['predictors'][:,irec,:] = data_buf[8:8+npred,:]           
        

        f.close()
        
        
        return diag_content
        
