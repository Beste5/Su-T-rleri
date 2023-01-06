--------------------------------------------------------------------------------------
#Hükümlü Ve Tutuklu Sayılarının Yıllar Bazında Cinsiyete Göre  Nokta Ve Çizgi Grafiği
--------------------------------------------------------------------------------------
  
#"ggplot2" paketi ve kütüphanesi kullanılabilmek için indirildi.
  
install.packages("ggplot2")
library(ggplot2)

# Cinsiyet adlı Veri seti kullanılabilmek için çağrıldı.

library(readxl)
Cinsiyet <- read_excel("C:/Users/BESTE ÜNAL/Desktop/Cinsiyet.xlsx")

# Cinsiyet adlı veri setinden alınan yıl ve hükümlü - tutuklu sayısı değişkeni
# kullanılarak Cinsiyete göre nokta ve çizgi grafiği birleştirildi.
# Cinsiyet "facet_wrap()" ile iki ayrı panele ayrıldı.
# Estetikler değiştirilerek okunurluk kazandırıldı.

ggplot(Cinsiyet, aes(x = yil, y = hukumlu_ve_tutuklu_sayisi, )) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = c(10000,50000,
                                100000,150000,200000,
                                250000,280000), labels = scales::comma) +
  labs(title = "Hükümlü Ve Tutuklu Sayılarının Yıllar 
Bazında Cinsiyete Göre Grafiği",
      y = "Hüküm Ve Tutuklu Sayıları", x = "Yıllar", 
      subtitle = "Çizgi Grafiği") +
  theme_bw() +
       facet_wrap(~cinsiyet)


------------------------------------------------------------------------
#Suç Türlerinin Eğitim Durumuna Göre Sayılarını Gösteren Çubuk Grafiği
------------------------------------------------------------------------
  
# Dönüşümleri yapabilmek ve grafiği çizdirebilmek için gerekli paketler ve
# kütüphaneler yüklendi.
# Eğitim düzeyi veri seti kulanılabilmek için çağrıldı.
# Eğitim düzeyi veri setinin içerisinde bulunan eğitim durumu değişkeninden "NA"
# değerleri çıkarıldı ve içerisinden belirli olanlar seçildi. Yeni eğitim durumu ile
# suç türleri değişkenleri grup haline getirilerek suçlu sayısı özetlenip
# "e" adında yeni bir veri setine atandı. Çubuk grafiği çizilerek estetiklerde düzenleme
# yapıldı.
  
install.packages("dplR")
install.packages("tidyverse")
install.packages("tidyr")
library(tidyverse)
library(dplyr)
library(tidyr) 


library(readxl)
egitim_duzeyi <- read_excel("C:/Users/BESTE ÜNAL/Desktop/egitim_duzeyi.xlsx")

e <- egitim_duzeyi %>%
  drop_na() %>%
  filter(egitim_durumu %in% c("İlköğretim", "Ortaokul Ve Dengi Meslek Okulu",
                              "Lise Ve Dengi Meslek okulu", "Yüksek Öğretim")) %>%
  group_by(suc_turleri, egitim_durumu) %>%
  summarise(suclu_sayısı)


ggplot(e, aes(y = suclu_sayısı, x = suc_turleri, fill = egitim_durumu)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Suç Türlerinin Yıllar Bazında 
Eğitim Durumlarına Göre Grafiği",
       fill = "Eğitim Durumu",
       y = "Suç Türleri", x = "Suçlu Sayısı", 
       subtitle = "Çubuk Grafiği") +
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = met.brewer("Degas"), labels = c("İlköğretim", "Ortaokul Ve Dengi Meslek Okulu",
                                                              "Lise Ve Dengi Meslek okulu", "Yüksek Öğretim ")) +
  coord_flip() +
  theme_bw()


--------------------------------------------------------------------------------
#Yaş Gruplarının Suçlu Sayısı ve Yıllara Göre Dağılımını Gösteren Kutu Grafiği
--------------------------------------------------------------------------------

# Yaş gruplarının bulunduğu veri seti kullanılması için çağrıldı.
  
  yas_gruplari <- read_excel("C:/Users/BESTE ÜNAL/Desktop/yas_gruplari.xlsx")
  
# Dağılımı gösterebilmek için bir kutu grafiği çizildi ve eksenler
# "coord_flip" yardımıyla yer değiştirilerek grafiğin okunurluğu arttırıldı.


 ggplot(yas_gruplari, aes(x=Yıl, y=Suclu_sayısı, fill = Yas_grubu)) +
  geom_bar(stat = "identity", position = "dodge") +
   scale_y_continuous(labels = scales::comma) +
   labs(x = "Yıl", y = "Suçlu Sayısı", fill = "Yaş Grubu",
        title = "2015-2020 Yılları Arasındakı Suçlu Sayılarının
Yaş Gruplarına Ayrılmış Grafiği",
        subtitle = "Kutu Grafiği") +
   facet_wrap(~Yas_grubu) +
   theme_light()
   
              

--------------------------------------------------------------------------------
# Medeni Duruma Göre Suçlu Sayısı Gösteren Grafik
-------------------------------------------------------------------------------- 
 
# Medeni durum veri seti kullanılmak için çağrıldı.

 library(readxl)
 Medeni_Durum <- read_excel("C:/Users/BESTE ÜNAL/Desktop/Medeni Durum.xlsx")
 

# Medeni durum veri setinden alınan Suçlu Sayısı ve medeni durum değişkenleri kullanılarak
# bir çubuk grafiği çizildi.
# Grafiğin okunabilmesi için Suçlu sayıları "scales::comma" komutu ile 
# büyük sayılar virgülle ayrılarak okunması kolaylaştırıldı.
 
 
 ggplot(Medeni_Durum, aes(x=medeni_durum, y=suclu_sayısı)) +
   geom_bar(stat = "identity", fill = "black") +
   scale_y_continuous(breaks = c(0, 25000,50000,75000,
                                 100000, 125000,150000,175000,200000, 225000,
                                 250000,275000,300000), labels = scales::comma) +
   labs(x = "Medeni Durum", y = "Suçlu Sayısı", fill = "Medeni Durum",
        title = "2015-2020 Yılları Arasındakı Suçlu Sayılarının
Medeni Durum Açısından İncelenmiş Grafiği",
        subtitle = "Çubuk Grafiği") +
   theme_light()
 
 
 
 
# Veri Seti Link
# " https://data.tuik.gov.tr/Bulten/Index?p=Ceza-Infaz-Kurumu-Istatistikleri-2020-37202""
 
 
 
 
 


























































