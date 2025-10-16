gage_binario = function(part, appr, result, rep, patt,datos) {
# Función programada en R
# Calcula exactitud, repetitividad y reproducibilidad para mediciones binarias
  # Hay 2 medidores (appr) que miden/clasifican varios objetos (part) en una 
  # escala binaria 0/1 (result). Cada objeto lo miden 2 veces cada uno (rep). 
  # Se comparan concordancias con clasificación correcta (patt), en escala 0/1.

  # Argumentos:
  # si las columnas de datos tienen estos nombres, solo es preciso especificar 'datos'
  # part=identificador del objeto
  # appr=identificador del observador/medidor/instrumento de medida
  # rep=réplica u ordinal de la observación (2 observaciones por observador)
  # result=catalogación del objeto por el observador (defectuoso/no defectuoso)
  # patt=catalogación (real) del objeto (defectuoso/no defectuoso)
  # datos= data.frame con todos los datos.
  
# Exactitud por observador: identificación correcta de defectos por cada observador
# Exactitud globa: identificación correcta de defectos sin distinguir observador
# Repetitividad por observador: concordancias entre las 2 mediciones de cada observador
# Repetitividad global : concordancias entre las 2 mediciones sin distinguir observador
# Reproducibilidad: concordancias entre todas las  mediciones de los dos observadores
  
n=length(unique(datos$part)) # número de productos medidos por cada medidor

# REPETITIVIDAD Y EXACTITUD POR OBSERVADOR
repetitividad = exactitud= vector() 
test_repetitividad =test_exactitud= list()
    
    for (i in 1:2) {
        # aislamos los datos del medidor i
        datos_appr = datos[datos$appr==i,]
        # REPETITIVIDAD por operador
        # contamos concordancias en sus mediciones
        repetitividad[i] = sum(datos_appr[datos_appr$rep==1,'result']==datos_appr[datos_appr$rep==2,'result'])
        print(i,repetitividad[i])
        # test H0: p=0.5 replicar una medición igual es cuestión de azar
        test_repetitividad[[i]] = prop.test(repetitividad[i], n) #$estimate # p.value, conf.int
        
        # EXACTITUD
        # calculamos la detección de defectos por operador
        # defectos reales
        n_defectos = sum((datos_appr[datos_appr$rep==1,'patt']==1)) 
        # defectos detectados (en ambas mediciones)
        exactitud[i] = sum((datos_appr[datos_appr$rep==1,'patt']==1) & 
                            (datos_appr[datos_appr$rep==1,'result']==1) &
                            (datos_appr[datos_appr$rep==2,'result']==1)  )
        # test H0: p=0.5 medir bien es cuestión de azar
        test_exactitud[[i]]=prop.test(exactitud[i], n_defectos) #$estimate # p.value, conf.int
    }
# EXACTITUD GLOBAL
# calculamos la detección de defectos sin distinguir por operador ni medición
exact_global = sum((datos$patt==1)&(datos$result==1))
test_exact_global=prop.test(exact_global, 4*n) #$estimate # p.value, conf.int

# REPETITIVIDAD GLOBAL
# comparamos las primeras mediciones (rep1) con las segundas (rep2)
rep1 = datos[datos$rep==1,'result']
rep2 = datos[datos$rep==2,'result']
repet_glob = sum(sum(rep1==rep2))
test_repet_global=prop.test(repet_glob, 2*n) #$estimate # p.value, conf.int

# REPRODUCIBILIDAD
# comparamos todas las mediciones el observador 1 con las del observador 2
reproduce = sum(datos$result[datos$appr == 1] == datos$result[datos$appr == 2])
test_reproducibilidad=prop.test(reproduce, 2*n) #$estimate # p.value, conf.int

# VOLCADO DE RESULTADOS
# Inicialización matriz resultados
d=rep(NA,7)
res=data.frame(Medida=d,Estim = d,IC95=d,p.value=d,H0=d)

# Volcado resultados exactitud
test = test_exactitud
for(i in 1:2){
  j=i
res[i,1]=paste("Exactitud Medidor",j)
res[i,2] = paste0(round(test[[j]]$estimate*100,2),"%")
res[i,3]= paste0("[",round(test[[j]]$conf.int[1]*100,2),"%, ",
                    round(test[[j]]$conf.int[2]*100,2),"%]")
res[i,4]=round(test[[j]]$p.value,4)
res[i,5]= test[[j]]$null.value   
}

# Volcado resultados exactitud global
test = test_exact_global
i=3
res[i,1]="Exactitud Global"
res[i,2] = paste0(round(test$estimate*100,2),"%")
res[i,3]= paste0("[",round(test$conf.int[1]*100,2),"%, ",
                    round(test$conf.int[2]*100,2),"%]")
res[i,4]=round(test$p.value,4)
res[i,5]= test$null.value    

# Volcado resultados repetitividad
test = test_repetitividad
for(i in 4:5){
  j=i-3
res[i,1]=paste("Repetitividad Medidor",j)
res[i,2] = paste0(round(test[[j]]$estimate*100,2),"%")
res[i,3]= paste0("[",round(test[[j]]$conf.int[1]*100,2),"%, ",
                    round(test[[j]]$conf.int[2]*100,2),"%]")
res[i,4]=round(test[[j]]$p.value,4)
res[i,5]= test[[j]]$null.value               
}

# Volcado resultados repetitividad global
test = test_repet_global
i=6
res[i,1]="Repetitividad Global"
res[i,2] = paste0(round(test$estimate*100,2),"%")
res[i,3]= paste0("[",round(test$conf.int[1]*100,2),"%, ",
                    round(test$conf.int[2]*100,2),"%]")
res[i,4]=round(test$p.value,4)
res[i,5]= test$null.value               


# Volcado resultados reproducibilidad
test = test_reproducibilidad
i=7
res[i,1]="Reproducibilidad"
res[i,2] = paste0(round(test$estimate*100,2),"%")
res[i,3]= paste0("[",round(test$conf.int[1]*100,2),"%, ",
                    round(test$conf.int[2]*100,2),"%]")
res[i,4]=round(test$p.value,4)
res[i,5]= test$null.value  

return(res)}
