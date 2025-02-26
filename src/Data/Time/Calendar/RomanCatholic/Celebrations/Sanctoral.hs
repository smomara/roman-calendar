module Data.Time.Calendar.RomanCatholic.Celebrations.Sanctoral where

import Data.Time (toGregorian)
import Data.Time.Calendar.RomanCatholic.Types

sanctoralCelebrations :: [CelebrationRule]
sanctoralCelebrations =
  concat
    [ januaryCelebrations
    , februaryCelebrations
    , marchCelebrations
    , aprilCelebrations
    , mayCelebrations
    , juneCelebrations
    , julyCelebrations
    , augustCelebrations
    , septemberCelebrations
    , octoberCelebrations
    , novemberCelebrations
    , decemberCelebrations
    ]

fixedDateRule :: Int -> Int -> String -> (String -> Celebration) -> LiturgicalColor -> Int -> CelebrationRule
fixedDateRule month day name celebType col rank =
  CelebrationRule
    { matchesDay = \(_, d) ->
        let (_, m, dm) = toGregorian d in m == month && dm == day
    , celebration = \_ _ -> celebType name
    , color = col
    , precedence = rank
    }

januaryCelebrations :: [CelebrationRule]
januaryCelebrations =
  [ fixedDateRule 1 1 "Mary, Mother of God" Solemnity White 3
  , fixedDateRule 1 2 "Saints Basil the Great and Greogry Nazianzen" Memorial White 10
  , fixedDateRule 1 3 "The Most Holy Name of Jesus" OptionalMemorial White 12
  , fixedDateRule 1 5 "Saint John Neumann" Memorial White 10
  , fixedDateRule 1 6 "Blessed André Bessette" OptionalMemorial White 12
  , fixedDateRule 1 7 "Saint Raymond of Penyafort" OptionalMemorial White 12
  , fixedDateRule 1 13 "Saint Hilary" OptionalMemorial White 12
  , fixedDateRule 1 17 "Saint Anthony" Memorial White 10
  , fixedDateRule 1 20 "Saint Fabian" OptionalMemorial Red 12
  , fixedDateRule 1 20 "Saint Sebastian" OptionalMemorial Red 12
  , fixedDateRule 1 21 "Saint Agnes" Memorial Red 10
  , fixedDateRule 1 22 "Saint Vincent" OptionalMemorial Red 12
  , fixedDateRule 1 24 "Saint Francis de Sales" Memorial White 10
  , fixedDateRule 1 25 "The Conversoin of Saint Paul" Feast White 7
  , fixedDateRule 1 26 "Saints Timothy and Titus" Memorial White 10
  , fixedDateRule 1 27 "Saint Angela Merici" OptionalMemorial White 12
  , fixedDateRule 1 28 "Saint Thomas Aquinas" Memorial White 10
  , fixedDateRule 1 31 "Saint John Bosco" Memorial White 10
  ]

februaryCelebrations :: [CelebrationRule]
februaryCelebrations =
  [ fixedDateRule 2 2 "The Presentation of the Lord" Feast White 5
  , fixedDateRule 2 3 "Saint Blase" OptionalMemorial Red 12
  , fixedDateRule 2 3 "Saint Ansgar" OptionalMemorial White 12
  , fixedDateRule 2 5 "Saint Agatha" Memorial Red 10
  , fixedDateRule 2 6 "Saint Paul Miki and Companions" Memorial Red 10
  , fixedDateRule 2 8 "Saint Jerome Emiliani" OptionalMemorial White 12
  , fixedDateRule 2 8 "Saint Josephine Bakhita" OptionalMemorial White 12
  , fixedDateRule 2 10 "Saint Scholastica" Memorial White 10
  , fixedDateRule 2 11 "Our Lady of Lourdes" OptionalMemorial White 12
  , fixedDateRule 2 14 "Saints Cyril and Methodius" Memorial White 10
  , fixedDateRule 2 17 "The Seven Holy Founders of the Servite Order" OptionalMemorial White 12
  , fixedDateRule 2 21 "Saint Peter Damian" OptionalMemorial White 12
  , fixedDateRule 2 22 "The Chair of Peter" Feast White 7
  , fixedDateRule 2 23 "Saint Polycarp" Memorial Red 10
  , fixedDateRule 2 27 "Saint Gregory of Narek" OptionalMemorial White 12
  ]

marchCelebrations :: [CelebrationRule]
marchCelebrations =
  [ fixedDateRule 3 4 "Saint Casimir" OptionalMemorial Green 12
  , fixedDateRule 3 7 "Saints Perpetua and Felicity" Memorial Red 10
  , fixedDateRule 3 8 "Saint John of God" OptionalMemorial White 12
  , fixedDateRule 3 9 "Saint Francis of Rome" OptionalMemorial White 12
  , fixedDateRule 3 17 "Saint Patrick" OptionalMemorial White 12
  , fixedDateRule 3 19 "Saint Cyril of Jerusalem" OptionalMemorial White 12
  , fixedDateRule 3 19 "Saint Joseph" Solemnity White 3
  , fixedDateRule 3 23 "Saint Turibius de Mogrojevo" OptionalMemorial White 12
  , fixedDateRule 3 25 "The Annunciation of the Lord" Solemnity White 3
  ]

aprilCelebrations :: [CelebrationRule]
aprilCelebrations =
  [ fixedDateRule 4 2 "Saint Francis of Paola" OptionalMemorial White 12
  , fixedDateRule 4 4 "Saint Isidore of Seville" OptionalMemorial White 12
  , fixedDateRule 4 5 "Saint Vincent Ferrer" OptionalMemorial White 12
  , fixedDateRule 4 7 "Saint John Baptist de la Salle" Memorial White 10
  , fixedDateRule 4 11 "Saint Stanislaus" OptionalMemorial Red 12
  , fixedDateRule 4 13 "Saint Martin I" OptionalMemorial Red 12
  , fixedDateRule 4 21 "Saint Anselm" OptionalMemorial White 12
  , fixedDateRule 4 23 "Saint George" OptionalMemorial Red 12
  , fixedDateRule 4 23 "Saint Adalbert" OptionalMemorial Red 12
  , fixedDateRule 4 24 "Saint Fidelis of Sigmaringen" OptionalMemorial Red 12
  , fixedDateRule 4 25 "Saint Mark" Feast Red 7
  , fixedDateRule 4 28 "Saint Peter Chanel" OptionalMemorial Red 12
  , fixedDateRule 4 28 "Saint Louis Mary de Montfort" OptionalMemorial White 12
  , fixedDateRule 4 29 "Saint Catherine of Siena" Memorial White 10
  , fixedDateRule 4 30 "Saint Pius V" OptionalMemorial White 12
  ]

mayCelebrations :: [CelebrationRule]
mayCelebrations =
  [ fixedDateRule 5 1 "Saint Joseph the Worker" OptionalMemorial White 12
  , fixedDateRule 5 2 "Saint Athanasius" Memorial White 10
  , fixedDateRule 5 3 "Saints Philip and James" Feast Red 7
  , fixedDateRule 5 10 "Saint John of Avila" OptionalMemorial White 10
  , fixedDateRule 5 12 "Saints Nereus and Achilleus" OptionalMemorial Red 12
  , fixedDateRule 5 12 "Saint Pancras" OptionalMemorial Red 12
  , fixedDateRule 5 13 "Our Lady of Fatima" OptionalMemorial White 12
  , fixedDateRule 5 14 "Saint Matthias" Feast Red 7
  , fixedDateRule 5 15 "Saint Isidore the Farmer" OptionalMemorial White 12
  , fixedDateRule 5 18 "Saint John I" OptionalMemorial White 12
  , fixedDateRule 5 20 "Saint Bernardine of Siena" OptionalMemorial White 12
  , fixedDateRule 5 21 "Saint Christopher Magallanes and Companions" OptionalMemorial Red 12
  , fixedDateRule 5 22 "Saint Rita of Cascia" OptionalMemorial White 12
  , fixedDateRule 5 25 "Saint Bede the Venerable" OptionalMemorial White 12
  , fixedDateRule 5 25 "Saint Gregory VII" OptionalMemorial White 12
  , fixedDateRule 5 25 "Saint Mary Magdalene de' Pazzi" OptionalMemorial White 12
  , fixedDateRule 5 26 "Saint Philip Neri" Memorial White 10
  , fixedDateRule 5 27 "Saint Augustine of Canterbury" OptionalMemorial White 12
  , fixedDateRule 5 31 "The Visitation of the Blessed Virgin Mary" Feast White 7
  ]

juneCelebrations :: [CelebrationRule]
juneCelebrations =
  [ fixedDateRule 6 1 "Saint Justin" Memorial Red 10
  , fixedDateRule 6 2 "Saints Marcellinus and Peter" OptionalMemorial Red 12
  , fixedDateRule 6 3 "Saint Charles Lwanga and Companions" Memorial Red 10
  , fixedDateRule 6 5 "Saint Boniface" Memorial Red 10
  , fixedDateRule 6 6 "Saint Norbert" OptionalMemorial White 12
  , fixedDateRule 6 9 "Saint Ephrem" OptionalMemorial White 12
  , fixedDateRule 6 11 "Saint Barnabas" Memorial Red 10
  , fixedDateRule 6 13 "Saint Anthony of Padua" Memorial White 10
  , fixedDateRule 6 19 "Saint Romuald" OptionalMemorial White 12
  , fixedDateRule 6 21 "Saint Aloysius Gonzaga" Memorial White 10
  , fixedDateRule 6 22 "Saint Paulinus of Nola" OptionalMemorial White 12
  , fixedDateRule 6 22 "Saints John Fisher and Thomas More" OptionalMemorial Red 12
  , fixedDateRule 6 24 "The Nativity of Saint John the Baptist" Solemnity White 3
  , fixedDateRule 6 27 "Saint Cyril of Alexandria" OptionalMemorial White 12
  , fixedDateRule 6 28 "Saint Irenaeus" OptionalMemorial Red 10
  , fixedDateRule 6 29 "Saints Peter and Paul" Solemnity Red 3
  , fixedDateRule 6 30 "The First Martyrs of the Holy Roman Church" OptionalMemorial Red 12
  ]

julyCelebrations :: [CelebrationRule]
julyCelebrations =
  [ fixedDateRule 7 1 "Blessed Junípero Serra" OptionalMemorial White 12
  , fixedDateRule 7 3 "Saint Thomas" Feast Red 7
  , fixedDateRule 7 4 "Saint Elizabeth of Portugal" OptionalMemorial White 12
  , fixedDateRule 7 5 "Saint Anthony Mary Zaccaria" OptionalMemorial White 12
  , fixedDateRule 7 6 "Saint Maria Goretti" OptionalMemorial Red 12
  , fixedDateRule 7 9 "Saint Augustine Zhao Rong and Companions" OptionalMemorial Red 12
  , fixedDateRule 7 11 "Saint Benedict" Memorial White 10
  , fixedDateRule 7 13 "Saint Henry" OptionalMemorial White 10
  , fixedDateRule 7 15 "Saint Bonaventure" Memorial White 10
  , fixedDateRule 7 16 "Our Lady of Mount Carmel" OptionalMemorial White 12
  , fixedDateRule 7 20 "Saint Apollinaris" OptionalMemorial Red 12
  , fixedDateRule 7 21 "Saint Lawrence of Brindisi" OptionalMemorial White 12
  , fixedDateRule 7 22 "Saint Mary Magdalene" Feast White 7
  , fixedDateRule 7 23 "Saint Bridget of Sweden" OptionalMemorial White 12
  , fixedDateRule 7 24 "Saint Sharbel Makhlūf" OptionalMemorial White 12
  , fixedDateRule 7 25 "Saint James" Feast Red 7
  , fixedDateRule 7 26 "Saints Joachim and Anne" Memorial White 10
  , fixedDateRule 7 29 "Saints Martha, Mary, and Lazarus" Memorial White 10
  , fixedDateRule 7 30 "Saint Peter Chrysologus" OptionalMemorial White 12
  , fixedDateRule 7 31 "Saint Ignatius of Loyola" Memorial White 10
  ]

augustCelebrations :: [CelebrationRule]
augustCelebrations =
  [ fixedDateRule 8 1 "Saint Alphonsus Liguori" Memorial White 10
  , fixedDateRule 8 2 "Saint Eusebius of Vercelli" OptionalMemorial White 12
  , fixedDateRule 8 2 "Saint Peter Julian Eymard" OptionalMemorial White 12
  , fixedDateRule 8 4 "Saint John Vianney" Memorial White 10
  , fixedDateRule 8 5 "The Dedication of the Basilica of Saint Mary Major" OptionalMemorial White 12
  , fixedDateRule 8 6 "The Transfiguration of the Lord" Feast White 5
  , fixedDateRule 8 7 "Saint Sixtus II and companions" OptionalMemorial Red 12
  , fixedDateRule 8 7 "Saint Cajetan" OptionalMemorial White 12
  , fixedDateRule 8 8 "Saint Dominic" Memorial White 10
  , fixedDateRule 8 9 "Saint Teresa Benedicta of the Cross" OptionalMemorial Red 12
  , fixedDateRule 8 10 "Saint Lawrence" Feast Red 7
  , fixedDateRule 8 11 "Saint Clare" Memorial White 10
  , fixedDateRule 8 12 "Saint Jane Frances de Chantal" OptionalMemorial White 12
  , fixedDateRule 8 13 "Saints Pontian and Hippolytus" OptionalMemorial Red 12
  , fixedDateRule 8 14 "Saint Maximilian Kolbe" Memorial Red 10
  , fixedDateRule 8 15 "The Assumption of the Blessed Virgin Mary" Solemnity White 3
  , fixedDateRule 8 16 "Saint Stephen of Hungary" OptionalMemorial White 12
  , fixedDateRule 8 19 "Saint John Eudes" OptionalMemorial White 12
  , fixedDateRule 8 20 "Saint Bernard" Memorial White 10
  , fixedDateRule 8 21 "Saint Pius X" Memorial White 10
  , fixedDateRule 8 22 "The Queenship of the Blessed Virgin Mary" Memorial White 10
  , fixedDateRule 8 23 "Saint Rose of Lima" OptionalMemorial White 12
  , fixedDateRule 8 24 "Saint Bartholomew" Feast Red 7
  , fixedDateRule 8 25 "Saint Louis" OptionalMemorial White 12
  , fixedDateRule 8 25 "Saint Joseph Calasanz" OptionalMemorial White 12
  , fixedDateRule 8 27 "Saint Monica" Memorial White 10
  , fixedDateRule 8 28 "Saint Augustine" Memorial White 10
  , fixedDateRule 8 29 "The Passion of Saint John the Baptist" Memorial Red 10
  ]

septemberCelebrations :: [CelebrationRule]
septemberCelebrations =
  [ fixedDateRule 9 3 "Saint Gregory the Great" Memorial White 10
  , fixedDateRule 9 5 "Saint Teresa of Calcutta" OptionalMemorial White 10
  , fixedDateRule 9 8 "The Nativity of the Blessed Virgin Mary" Feast White 7
  , fixedDateRule 9 12 "The Most Holy Name of Mary" OptionalMemorial White 12
  , fixedDateRule 9 13 "Saint John Chrysostom" Memorial White 10
  , fixedDateRule 9 14 "The Exaltation of the Holy Cross" Memorial White 5
  , fixedDateRule 9 15 "Our Lady of Sorrows" Memorial White 10
  , fixedDateRule 9 16 "Saints Cornelius and Cyprian" Memorial Red 10
  , fixedDateRule 9 17 "Saint Robert Bellarmine" OptionalMemorial White 12
  , fixedDateRule 9 17 "Saint Hildegard of Bingen" OptionalMemorial White 12
  , fixedDateRule 9 19 "Saint Januarius" OptionalMemorial Red 12
  , fixedDateRule 9 20 "Saints Andrew Kim Taegon, Paul Chong Hasang, and companions" Memorial Red 10
  , fixedDateRule 9 21 "Saint Matthew" Feast Red 7
  , fixedDateRule 9 23 "Saint Pius of Pietrelcina" Memorial White 10
  , fixedDateRule 9 26 "Saints Cosmas and Damian" OptionalMemorial Red 12
  , fixedDateRule 9 27 "Saint Vincent de Paul" Memorial White 10
  , fixedDateRule 9 28 "Saint Wenceslaus" OptionalMemorial Red 12
  , fixedDateRule 9 28 "Saint Lawrence Ruiz and companions" OptionalMemorial Red 12
  , fixedDateRule 9 29 "Saints Michael, Gabriel, and Raphael" Feast White 7
  , fixedDateRule 9 30 "Saint Jerome" Memorial White 10
  ]

octoberCelebrations :: [CelebrationRule]
octoberCelebrations =
  [ fixedDateRule 10 1 "Saint Thérèse of the Child Jesus" Memorial White 10
  , fixedDateRule 10 2 "The Holy Guardian Angels" Memorial White 10
  , fixedDateRule 10 4 "Saint Francis of Assisi" Memorial White 10
  , fixedDateRule 10 6 "Saint Bruno" OptionalMemorial White 10
  , fixedDateRule 10 6 "Blessed Marie Rose Durocher" OptionalMemorial White 12
  , fixedDateRule 10 7 "Our Lady of the Rosary" Memorial White 10
  , fixedDateRule 10 9 "Saint Denis and Companions" OptionalMemorial Red 12
  , fixedDateRule 10 9 "Saint John Leonardi" OptionalMemorial White 12
  , fixedDateRule 10 11 "Saint John XXIII" OptionalMemorial White 12
  , fixedDateRule 10 14 "Saint Callistus I" OptionalMemorial Red 12
  , fixedDateRule 10 15 "Saint Teresa of Avila" Memorial White 10
  , fixedDateRule 10 16 "Saint Hedwig" OptionalMemorial White 12
  , fixedDateRule 10 16 "Saint Margaret Mary Alocoque" OptionalMemorial White 12
  , fixedDateRule 10 17 "Saint Ignatius of Antioch" Memorial Red 10
  , fixedDateRule 10 18 "Saint Luke" Feast Red 7
  , fixedDateRule 10 19 "Saints Isaac Jogues and John de Brébeuf and Companions" Memorial Red 10
  , fixedDateRule 10 20 "Saint Paul of the Cross" OptionalMemorial White 12
  , fixedDateRule 10 23 "Saint John of Capistrano" OptionalMemorial White 12
  , fixedDateRule 10 28 "Saints Simon and Jude" Feast Red 7
  ]

novemberCelebrations :: [CelebrationRule]
novemberCelebrations =
  [ fixedDateRule 11 1 "All Saints" Solemnity White 3
  , fixedDateRule 11 2 "The Commemoration of All the Faithful Departed (All Souls)" Feast White 7
  , fixedDateRule 11 3 "Saint Martin de Porres" OptionalMemorial White 12
  , fixedDateRule 11 4 "Saint Charles Borromeo" Memorial White 10
  , fixedDateRule 11 9 "The Dedication of the Lateran Basilica" Feast White 7
  , fixedDateRule 11 10 "Saint Leo the Great" Memorial White 10
  , fixedDateRule 11 11 "Saint Martin of Tours" Memorial White 10
  , fixedDateRule 11 12 "Saint Josaphat" Memorial Red 10
  , fixedDateRule 11 15 "Saint Albert the Great" OptionalMemorial White 12
  , fixedDateRule 11 16 "Saint Margaret of Scotland" OptionalMemorial White 12
  , fixedDateRule 11 16 "Saint Gertrude the Great" OptionalMemorial White 12
  , fixedDateRule 11 17 "Saint Elizabeth of Hungary" Memorial White 10
  , fixedDateRule 11 18 "The Dedication of the Basilicas of Saints Peter and Paul" OptionalMemorial White 12
  , fixedDateRule 11 21 "The Presentation of the Blessed Virgin Mary" Memorial White 10
  , fixedDateRule 11 22 "Saint Cecilia" Memorial Red 10
  , fixedDateRule 11 23 "Saint Clement I" OptionalMemorial Red 12
  , fixedDateRule 11 23 "Saint Columban" OptionalMemorial White 12
  , fixedDateRule 11 23 "Saint Miguel Agustin Pro" OptionalMemorial Red 12
  , fixedDateRule 11 24 "Saint Andrew Dũng-Lạc and Companions" Memorial Red 10
  , fixedDateRule 11 25 "Saint Catherine of Alexandria" OptionalMemorial Red 12
  , fixedDateRule 11 30 "Saint Andrew" Feast Red 7
  ]

decemberCelebrations :: [CelebrationRule]
decemberCelebrations =
  [ fixedDateRule 12 3 "Saint Francis Xavier" Memorial White 10
  , fixedDateRule 12 4 "Saint John Damascene" OptionalMemorial White 12
  , fixedDateRule 12 6 "Saint Nicholas" OptionalMemorial White 12
  , fixedDateRule 12 7 "Saint Ambrose" Memorial White 10
  , fixedDateRule 12 8 "The Immaculate Conception of the Blessed Virgin Mary" Solemnity White 3
  , fixedDateRule 12 9 "Saint Juan Diego" OptionalMemorial White 12
  , fixedDateRule 12 10 "Our Lady of Loreto" OptionalMemorial White 10
  , fixedDateRule 12 11 "Saint Damasus I" OptionalMemorial White 12
  , fixedDateRule 12 13 "Saint Lucy" Memorial Red 10
  , fixedDateRule 12 14 "Saint John of the Cross" Memorial White 10
  , fixedDateRule 12 21 "Saint Peter Canisius" OptionalMemorial White 12
  , fixedDateRule 12 23 "Saint John of Kanty" OptionalMemorial White 12
  , fixedDateRule 12 26 "Saint Stephen" Feast Red 7
  , fixedDateRule 12 27 "Saint John" Feast White 7
  , fixedDateRule 12 28 "The Holy Innocents" Feast Red 7
  , fixedDateRule 12 29 "Saint Thomas Becket" OptionalMemorial Red 12
  , fixedDateRule 12 31 "Saint Sylvester I" OptionalMemorial White 12
  ]
