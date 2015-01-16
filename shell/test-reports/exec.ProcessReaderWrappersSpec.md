## ProcessReaderWrappers

`ProcessReaderWrappers` trait                                                                          
   reimplements `ExecUtils`'s execution methods the way the process input streams would be available (+)  
    provides wrapping to `RunningProcessReader`, that                                                


      reads process streams asynchronously                                                            
             output (+)  
             error (+)  
       may be represented as `Future` (+)  
      upon process completion constructs `FinishedProcessWithReadStreams`, that                       

        
                is accessed from the corresponding `RunningProcessReader`                                               
                    synchronously (.await)                                                                                  
                                    for successful execution (exit code = 0) (+)            
                                    for unsuccessful execution (exit code != 0) (+)          
                    asynchronously (onCompleteReading/onSuccessReading/onFailureReading)                                    
                                    for successful execution (exit code = 0) (+)            
                                    for unsuccessful execution (exit code != 0) (+)        
                contains full streams output                                                                            
                              output (+)          
                              error (+)                                                                                                      
                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                            
| ProcessReaderWrappers |
| Finished in 18 ms |
| 10 examples, 0 failure, 0 error |