## ProcessWrappers
  
   `ProcessWrappers` trait provides                                                
        `RunningProcess` and an conversion `.wrap` from `Process`                 


              A `RunningProcess` listens to process exit and creates a `FinishedProcess`, that can be accessed              
            
                        Synchronously                                                                      
                              by method `await` (+)                                                                        
                                                                                                                                                                                                                                                                                                            
                        Asynchronously                                                                    
                              by method `onComplete` (+)          
                              by methods `onSuccess`/`onFailure` (+)                                                                        
                                                                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                    
        `FinishedProcess` container                                                                                                                             
                                                                                                                                                            
| ProcessWrappers |
| Finished in 17 ms |
| 3 examples, 0 failure, 0 error |