# ClusteringMethods
Foundations of Hierarchical Clustering


[GitHub](https://github.com/DeSlowe/ClusteringMethods.git)

=======
>>>>>>> 40ccd75d5f215828cfa4d718bb08b74d86eefddf
```{r setup, include=FALSE}
knitr::opts_chunk$set(
  eval = TRUE,    # Ensure all chunks execute
  echo = TRUE,    # Show the code in the output
  warning = FALSE, # Suppress warnings
  message = FALSE  # Suppress messages
)
```


# **Introduzione al Clustering Gerarchico**

Il **clustering gerarchico** è una tecnica di raggruppamento non supervisionato che costruisce una gerarchia di cluster. Può essere rappresentato attraverso un dendrogramma, che mostra la relazione tra i diversi cluster.

## Perché è utile?
- Non richiede di specificare a priori il numero di cluster.
- Fornisce una rappresentazione gerarchica che può essere tagliata a diversi livelli per ottenere diversi numeri di cluster.
- Utile per l'analisi esplorativa dei dati.

```{r}
# Loading packages
library(tidyverse)
library(patchwork)
library(dplyr)
library(plyr)
library(plotly)
library(GGally)
library(dendextend)
library(factoextra)
library(fmsb)
library(formattable)
```
# **Data**
I dati fanno parte di un campione di clienti di un supermercato, ogni osservazione rappresenta un cliente, le informazioni riguardano:

- **Age**: l'età del cliente
- **Gender**: il sesso del cliente
- **Income**: il reddito del cliente
- **Score**: un punteggio relativo al livello di spesa


```{r}
setwd("..\\data")
# Load data
data = read.csv("mall_customers.csv")
# Check for any missing value
if (anyNA(data)) {
  print("Warning! There are missing values")
}
# rename columns
new_colnames = c('Id', 'Gender', 'Age', 'Income', 'Score')
names(data) = new_colnames
head(data)
```

```{r}
# Standardize quantitative variables
data$Age_std = scale(data$Age)[,1]
data$Income_std = scale(data$Income)[,1]
data$Score_std = scale(data$Score)[,1]
```


# **Misure di Distanza**

Le misure di distanza determinano come vengono calcolate le somiglianze tra gli elementi:

- **Distanza Euclidea**: $d(x,y) = \sqrt{\sum_{i=1}^{n} (x_i - y_i)^2}$
- **Distanza di Manhattan**: $d(x,y) = \sum_{i=1}^{n} |x_i - y_i|$
- **Distanza di Minkowski**: $d(x,y) = \left(\sum_{i=1}^{n} |x_i - y_i|^p \right)^{\frac{1}{p}}$

Dove la distanza di Minkowski generalizza le altre due: per $p=2$ si ottiene la distanza euclidea, per $p=1$ la distanza di Manhattan.

```{r}
# Calculate distances matrix
distance_matrix = data %>% 
  select(Income_std, Score_std) %>% 
  dist(method = 'euclidean')
```

# **Metodi di Collegamento (Linkage)**

Il **linkage**, o metodo di collegamento, è la strategia utilizzata nel **clustering gerarchico** per determinare la distanza tra due cluster durante il processo di fusione. Quando eseguiamo il clustering gerarchico, iniziamo con ogni punto come un cluster separato e successivamente li aggreghiamo passo dopo passo. La scelta del metodo di linkage influenza in modo significativo la struttura dei cluster e il risultato finale.

## Come funziona il linkage?

Ad ogni iterazione, il metodo di linkage determina quale coppia di cluster unire sulla base della distanza tra di loro. La distanza tra due cluster dipende da come definiamo la distanza tra i punti appartenenti ai due cluster.

**1. Single Linkage (collegamento singolo)**

La distanza tra due cluster è la distanza minima tra tutti i punti nei due cluster.

Favorisce cluster allungati.

Sensibile agli outlier e al fenomeno del chaining (formazione di catene di punti).

**2. Complete Linkage (collegamento completo)**

La distanza tra due cluster è la distanza massima tra tutti i punti nei due cluster.

Crea cluster compatti e sferici.

Meno sensibile al chaining rispetto al Single Linkage.

**3. Average Linkage (collegamento medio)**

La distanza tra due cluster è la media delle distanze tra tutti i punti nei due cluster.

Bilanciato tra complete e single linkage.

Tende a formare cluster di dimensioni simili.

**4. Ward’s Linkage (metodo di Ward)**

Minimizza la varianza totale all'interno dei cluster dopo la fusione.

Tende a formare cluster compatti e simmetrici.

Adatto a dati con struttura ben definita.


```{r}
# Single Linkage
hc1 = hclust(distance_matrix, method = 'single') %>% 
  as.dendrogram() %>% 
  set('labels', '') # Avoid to plot labels
plot(hc1, main = "Hierarchical Clustering: Single Linkage")


# Complete Linkage
hc2 = hclust(distance_matrix, method = 'complete') %>% 
  as.dendrogram() %>% 
  set('labels', '') # Avoid to plot labels
plot(hc2, main = "Hierarchical Clustering: Complete Linkage")

# Average Linkage
hc3 = hclust(distance_matrix, method = 'average') %>% 
  as.dendrogram() %>% 
  set('labels', '') # Avoid to plot labels 
plot(hc3, main = "Hierarchical Clustering: Average Linkage")

# Ward's Method
hc4 = hclust(distance_matrix, method = 'ward.D') %>% 
  as.dendrogram() %>% 
  set('labels', '') # Avoid to plot labels 
plot(hc4, main = "Hierarchical Clustering: Ward's Method")

```

---

# **Relazione tra Linkage e Misure di Distanza**

La scelta della misura di distanza influisce sulla matrice di distanza iniziale, mentre il linkage determina come questi valori vengono aggregati nei cluster.

| **Linkage** | **Distanza consigliata** | **Caratteristiche** |
|------------|------------------|------------------|
| **Single** | Manhattan, Euclidea | Sensibile agli outlier, tende a creare cluster allungati |
| **Complete** | Euclidea, Minkowski | Preferibile per cluster compatti, meno sensibile agli outlier |
| **Average** | Manhattan, Euclidea | Bilanciato, adatto a cluster di forma variabile |
| **Ward** | **Solo Euclidea** | Minimizza la varianza, forma cluster sferici e ben separati |

### Considerazioni sulla scelta della combinazione distanza-linkage:
- **Ward richiede obbligatoriamente la distanza euclidea**, poiché si basa sulla minimizzazione della varianza.
- Gli altri metodi possono essere usati con qualsiasi distanza, ma la scelta influisce sul risultato finale.

### Suggerimenti per la scelta:
- **Se i cluster sono ben separati e compatti** → **Complete linkage + Euclidea**  
- **Se i cluster sono allungati o formano catene** → **Single linkage + Manhattan**  
- **Se serve un compromesso tra compattezza e flessibilità** → **Average linkage + Manhattan**  
- **Se si vuole una divisione ottimale basata sulla varianza** → **Ward + Euclidea**  

---

# **Metodi per la Scelta del Numero Ottimale di Cluster**

Per determinare il numero ottimale di cluster si possono usare diversi metodi:

## Metodo della Silhouette

La silhouette misura quanto un punto è simile ai punti del proprio cluster rispetto ai punti degli altri cluster. Un valore alto indica una buona assegnazione.

## WSS (Within Sum of Squares)

L'analisi dell'**Elbow Method** osserva la somma dei quadrati delle distanze all'interno dei cluster (WSS) in funzione del numero di cluster. Il punto di ginocchio indica il numero ottimale di cluster.

## Gap Statistic

Il **Gap Statistic** confronta la dispersione nei cluster ottenuti con quella attesa in un riferimento casuale. Il numero ottimale di cluster è quello che massimizza la differenza tra i due.

```{r}
# 1. Elbow Method: Minimizing within cluster sums of squares
# One should choose a number of clusters so that adding another cluster 
# doesn't improve much better the total WSS
data %>% 
  select(Income_std, Score_std) %>%
  fviz_nbclust(FUN = hcut, method = 'wss')
```

```{r}
# 2. Average Silhouette Method: measuring the quality of clustering
# It determines how well each object lies within its cluster. 
# A high average silhouette width indicates a good clustering
data %>% 
  select(Income_std, Score_std) %>% 
  scale() %>% 
  fviz_nbclust(FUN = hcut, method = 'silhouette')
```

```{r}
# Hierarchical clustering
par(mfrow=c(1,1))

# Single Linkage
hc1 = hclust(distance_matrix, method = 'single') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% # Avoid to plot labels
  set('branches_k_color', k = 5)
plot(hc1, main = "Hierarchical Clustering: Single Linkage")

# Complete Linkage
hc2 = hclust(distance_matrix, method = 'complete') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% # Avoid to plot labels
  set('branches_k_color', k = 5)
plot(hc2, main = "Hierarchical Clustering: Complete Linkage")

# Average Linkage
hc3 = hclust(distance_matrix, method = 'average') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% # Avoid to plot labels 
  set('branches_k_color', k = 5)
plot(hc3, main = "Hierarchical Clustering: Average Linkage")

# Ward's Method
hc4 = hclust(distance_matrix, method = 'ward.D') %>% 
  as.dendrogram() %>% 
  set('labels', '') %>% # Avoid to plot labels 
  set('branches_k_color', k = 5)
plot(hc4, main = "Hierarchical Clustering: Ward's Method")
```


```{r}
# Plot Clusters by Income and Score

# Cluster: Single linkage
clust1 = cutree(hc1, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Single Linkage")

# Cluster: Complete Linkage
clust2 = cutree(hc2, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Complete Linkage")

# CLuster: Average Linkage
clust3 = cutree(hc3, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Average Linkage")

# Cluster: Ward's Method
clust4 = cutree(hc4, k = 5) %>% 
  data.frame() %>% 
  cbind(data) %>% 
  ggplot() + 
  geom_point(aes(x = Income,
                 y = Score,
                 color = as.factor(.)),
             show.legend = F) +
  labs(title = "Ward's Method")

# Multiple plot
clust1 + clust2 + clust3 + clust4
```


# **Conclusione**

Il clustering gerarchico è una tecnica potente per esplorare la struttura dei dati e identificare gruppi naturali senza dover specificare a priori il numero di cluster. La scelta del metodo di collegamento e della metrica di distanza influisce sui risultati e dovrebbe essere fatta in base ai dati e agli obiettivi dell'analisi.


