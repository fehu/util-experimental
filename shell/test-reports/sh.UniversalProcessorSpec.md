## UniversalProcessor
  
     A `UniversalProcessor` processes source code, reading its configuration from the same source.
     Provides the following features:                                                            
                        | sh-line |   processes one-line shell expressions (+)        
                        the 'one-line' shell expressions support '\' multi-lining (+)        
                        | sh-block |  processes multi-line shell expression blocks (+)            
            
                        | shortcuts | provide the following shortcuts for scala expressions                         
                                                var:    $name = ...      => var name = ... (+)                
                                                val:    c$name = ...     => val name = ... (+)                
                                                args:   $1, $2, ..., $N  => args(1), args(2), ..., args(N) (+)                
                                                object: ##name           => object name (+)            
                        shortcuts must not affect strings and the following expressions                             
                                                var:    "$arg = $value" (+)                
                                                val:    "c$arg = c$value" (+)                
                                                args:   evidence$1, evidence$2 (+)                
                                                object: x.## max 2, "##ERROR" (+)            
                        Multiline config:                                                                           
                                                several #conf keywords in the begining of the source (+)                
                                                several #conf keywords in different parts of the source (+)                
                                                multi-line, escaped by '\' (+)        
                        | #all |      key for enabling all the features listed above (+)            
            
                        Dependency management                                                                       
                                                by package *name*, *group* and *version* (+)                    
                                                            supports scala versioning (+)                
                                                by package *name* and *group*, choosing latest version (+)                    
                                                            supports scala versioning (+)                
                                                by package *name* only (+)                    
                                                            supports scala versioning (+)        
        
                Quick imports by predefined keys:                                                               
                                                'file'       => feh.util.FileUtils._ (+)                
                                                'exec'       => feh.util.ExecUtils._ (+)                
                                                'akka'       => akka.actor._, akka.pattern.ask, akka.event.Logging, (+)                                
                                                                scala.concurrent._, scala.concurrent.duration._                 
        
                Predefined imports                                                              
                        import feh.util._
        
                Predefined dependencies:                                                                    
                        org.scala-lang % scala-library % 2.11.5
                        feh.util %% util % 1.0.6
            
                                                                                                    
| UniversalProcessor |
| Finished in 2 ms |
| 24 examples, 25 expectations, 0 failure, 0 error |