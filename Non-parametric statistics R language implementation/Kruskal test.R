drug = c(80,203,236,252,284,368,457,393,133,180,100,160,156,
295,320,448,465,481,279,194,214,272,330,386,478)
gr.drug = c(rep(1,8),rep(2,4),rep(3,7),rep(4,6))
kruskal.test(drug,gr.drug)
