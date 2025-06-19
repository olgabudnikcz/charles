

# TPS grids -- SHAPE REGRESSION VISUALIZATIONS

# Overall SShD

## CMR

M <- mshape(pop.sh$CMR)
preds <- shape.predictor(pop.sh$CMR, cmr.sshd$sc, 
                          Intercept = TRUE,
                          predmin = min(cmr.sshd$sc),
                          predmax = max(cmr.sshd$sc))
                          
                          par(mfrow = c(1, 3), mar = c(0, 0, 2, 0),oma = c(0,2,0,0))
                          plotRefToTarget(M, preds$predmin, mag= 1, gridPars = GP2, links = links)
                          title ("LOW", cex.main = 1.5)
                          mtext("SShD",2 , cex = 1)
                          plotRefToTarget(M, M, mag= 1, gridPars = GP2, links = links)
                          title ("MEAN", cex.main = 1.5)
                          plotRefToTarget(M, preds$predmax, mag= 1, gridPars = GP2,links = links)
                          title ("HIGH", cex.main = 1.5)

## CZ
                          
M <- mshape(pop.sh$CZ)
preds <- shape.predictor(pop.sh$CZ, cz.sshd$sc, 
                          Intercept = TRUE,
                          predmin = min(cz.sshd$sc),
                          predmax = max(cz.sshd$sc))
                          
                          par(mfrow = c(1, 3), mar = c(0, 0, 2, 0),oma = c(0,2,0,0))
                          plotRefToTarget(M, preds$predmin, mag= 1, gridPars = GP2, links = links)
                          title ("LOW", cex.main = 1.5)
                          mtext("SShD",2 , cex = 1)
                          plotRefToTarget(M, M, mag= 1, gridPars = GP2, links = links)
                          title ("MEAN", cex.main = 1.5)
                          plotRefToTarget(M, preds$predmax, mag= 1, gridPars = GP2,links = links)
                          title ("HIGH", cex.main = 1.5)                          
                          
# Various MORDs

 ### CMR women
 M <- mshape(cmr.f.gdf$coords)
 preds <- shape.predictor(cmr.f.gdf$coords, x= cmr.f.gdf$v.mord.sshd, 
                          Intercept = TRUE,
                          predmin = min(cmr.f.gdf$v.mord.sshd),
                          predmax = max(cmr.f.gdf$v.mord.sshd))
                          
                          par(mfrow = c(1, 3), mar = c(0, 0, 2, 0),oma = c(0,2,0,0))
                          plotRefToTarget(M, preds$predmin, mag= 2, gridPars = GP2, links = links)
                          title ("LOW", cex.main = 1.5)
                          mtext("UP–LW MORD",2 , cex = 1)
                          plotRefToTarget(M, M, mag= , gridPars = GP2, links = links)
                          title ("MEAN", cex.main = 1.5)
                          plotRefToTarget(M, preds$predmax, mag= 2, gridPars = GP2,links = links)
                          title ("HIGH", cex.main = 1.5)
 
 
 preds <- shape.predictor(cmr.f.gdf$coords, x= cmr.f.gdf$lat.mord.sshd, 
                          Intercept = TRUE,
                          predmin = min(cmr.f.gdf$lat.mord.sshd),
                          predmax = max(cmr.f.gdf$lat.mord.sshd))
 
                          par(mfrow = c(1, 3), mar = c(0, 0, 2, 0),oma = c(0,2,0,0))
                          plotRefToTarget(M, preds$predmin, mag= 2, gridPars = GP2, links = links)
                          title ("LOW", cex.main = 1.5)
                          mtext("L–R MORD",2 , cex = 1)
                          plotRefToTarget(M, M, mag = 2, gridPars = GP2, links = links)
                          title ("MEAN", cex.main = 1.5)
                          plotRefToTarget(M, preds$predmax, mag= 2, gridPars = GP2, links = links)
                          title ("HIGH", cex.main = 1.5)
                          
                          
 preds <- shape.predictor(cmr.f.gdf$coords, x= cmr.f.gdf$avlw.mord.sshd, 
                          Intercept = TRUE,
                          predmin = min(cmr.f.gdf$avlw.mord.sshd),
                          predmax = max(cmr.f.gdf$avlw.mord.sshd))
                          
                          par(mfrow = c(1, 3), mar = c(0, 0, 2, 0),oma = c(0,2,0,0))
                          plotRefToTarget(M, preds$predmin, mag=3, gridPars = GP2, links = links)
                          title ("LOW", cex.main = 1.5)
                          mtext("AV–LW MORD",2 , cex = 1)
                          plotRefToTarget(M, M, mag=3, gridPars = GP2, links = links)
                          title ("MEAN", cex.main = 1.5)
                          plotRefToTarget(M, preds$predmax, mag=3, gridPars = GP2, links = links)
                          title ("HIGH", cex.main = 1.5)

                          
  preds <- shape.predictor(cmr.f.gdf$coords, x= cmr.f.gdf$avup.mord.sshd, 
                          Intercept = TRUE,
                          predmin = min(cmr.f.gdf$avup.mord.sshd),
                          predmax = max(cmr.f.gdf$avup.mord.sshd))
                          
                          par(mfrow = c(1, 3), mar = c(0, 0, 2, 0),oma = c(0,2,0,0))
                          plotRefToTarget(M, preds$predmin, mag=3, gridPars = GP2, links = links)
                          title ("LOW", cex.main = 1.5)
                          mtext("AV–UP MORD",2 , cex = 1)
                          plotRefToTarget(M, M, mag=3, gridPars = GP2, links = links)
                          title ("MEAN", cex.main = 1.5)
                          plotRefToTarget(M, preds$predmax, mag=3, gridPars = GP2, links = links)
                          title ("HIGH", cex.main = 1.5)        

### CMR men

  M <- mshape(cmr.m.gdf$coords)
  preds <- shape.predictor(cmr.m.gdf$coords, x= cmr.m.gdf$v.mord.sshd, 
                         Intercept = TRUE,
                          predmin = min(cmr.m.gdf$v.mord.sshd),
                          predmax = max(cmr.m.gdf$v.mord.sshd))                                         
                          
                          par(mfrow = c(1, 3), mar = c(0, 0, 2, 0),oma = c(0,2,0,0))
                          plotRefToTarget(M, preds$predmin, mag= 2, gridPars = GP2, links = links)
                          title ("LOW", cex.main = 1.5)
                          mtext("UP–LW MORD",2 , cex = 1)
                          plotRefToTarget(M, M, mag= 2, gridPars = GP2, links = links)
                          title ("MEAN", cex.main = 1.5)
                          plotRefToTarget(M, preds$predmax, mag= 2, gridPars = GP2,links = links)
                          title ("HIGH", cex.main = 1.5)
####  ABS -- CMR men                    
                          
M <- mshape(cmr.m.gdf$coords)
preds <- shape.predictor(cmr.m.gdf$coords, x= abs(cmr.m.gdf$v.mord.sshd), 
                          Intercept = TRUE,
                          predmin = min(cmr.m.gdf$v.mord.sshd),
                          predmax = max(cmr.m.gdf$v.mord.sshd))                                                  
                          
                          par(mfrow = c(1, 3), mar = c(0, 0, 2, 0),oma = c(0,2,0,0))
                          plotRefToTarget(M, preds$predmin, mag= 2, gridPars = GP2, links = links)
                          title ("LOW", cex.main = 1.5)
                          mtext("UP–LW MORD ABS",2 , cex = 1)
                          plotRefToTarget(M, M, mag= 2, gridPars = GP2, links = links)
                          title ("MEAN", cex.main = 1.5)
                          plotRefToTarget(M, preds$predmax, mag= 2, gridPars = GP2,links = links)
                          title ("HIGH", cex.main = 1.5)
                          
                          
preds <- shape.predictor(cmr.m.gdf$coords, x= cmr.m.gdf$lat.mord.sshd, 
                          Intercept = TRUE,
                          predmin = min(cmr.m.gdf$lat.mord.sshd),
                          predmax = max(cmr.m.gdf$lat.mord.sshd))
                          
                          par(mfrow = c(1, 3), mar = c(0, 0, 2, 0),oma = c(0,2,0,0))
                          plotRefToTarget(M, preds$predmin, mag= 2, gridPars = GP2, links = links)
                          title ("LOW", cex.main = 1.5)
                          mtext("L–R MORD",2 , cex = 1)
                          plotRefToTarget(M, M, mag = 2, gridPars = GP2, links = links)
                          title ("MEAN", cex.main = 1.5)
                          plotRefToTarget(M, preds$predmax, mag= 2, gridPars = GP2, links = links)
                          title ("HIGH", cex.main = 1.5)                          
                         
                          
                          
                          
                          
                                              
 