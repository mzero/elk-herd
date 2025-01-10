module Elektron.Digitakt.FactorySamples exposing
  ( filesByHash
  )

{-| Factory sample list.

As of OS 1.05 the Digitakt stopped reporting the factory samples via the API.
Fortunately, I had snagged a copy of the tree, and here it is!

It is important to have them so that items in the sample pool can be matched
to +Drive entries, even if they are using factory content, and we can show the
sample names.

I have no idea if this content list has remained stable in subsequent releases
but I suspect it has.

TODO: Plead with Elektron to supply the factory sample listings via the API.
They don't need to allow downloading the actual samples (what I suspect they
are trying to avoid), but we need to directory entries for the names, sizes
and hashes.
-}

import Dict

import Elektron.Drive as Drive
import Elektron.Path as Path exposing (Path)



mkPath : String -> Path
mkPath = List.reverse << List.filter (not << String.isEmpty) << String.split "/"

filesByHash : Drive.FilesByHash
filesByHash =
  let
    item dirPath { hash, size, name }
      = ((hash, size), [Path.subPath dirPath name])

    items (dir, infos) = List.map (item <| mkPath dir) infos

    allItems = List.concatMap items rawSampleData
  in
    Dict.fromList allItems
      -- this code assumes there are no collisions in the factory data


type alias Info = { hash : Int, size : Int, name : String }

rawSampleData : List (String, List Info)
rawSampleData =
  [ ( "/factory/Synths/Analog Notes"
    , [ { hash = 789033197, size = 193716, name = "Acidic" }
      , { hash = 1635520009, size = 348870, name = "Bronze" }
      , { hash = 3520956689, size = 194798, name = "Cheerful" }
      , { hash = 2820940849, size = 416398, name = "Cheers" }
      , { hash = 3405238297, size = 144024, name = "Cities" }
      , { hash = 1833914761, size = 342466, name = "Club" }
      , { hash = 1371401055, size = 138882, name = "Colorful" }
      , { hash = 3394510037, size = 247350, name = "Darker" }
      , { hash = 3343029285, size = 338670, name = "Determined" }
      , { hash = 1557967729, size = 73360, name = "Down" }
      , { hash = 854502379, size = 230464, name = "Effit" }
      , { hash = 117982693, size = 248218, name = "Falling" }
      , { hash = 859822275, size = 260240, name = "Felt" }
      , { hash = 2049737287, size = 53868, name = "Flaunt" }
      , { hash = 945097417, size = 247146, name = "Flies" }
      , { hash = 1910133291, size = 52386, name = "Franc" }
      , { hash = 1444615073, size = 114316, name = "Frog" }
      , { hash = 1878380003, size = 105086, name = "Golden" }
      , { hash = 489965405, size = 196864, name = "Highly" }
      , { hash = 2909980347, size = 108794, name = "Hits" }
      , { hash = 98955103, size = 194992, name = "Kitty" }
      , { hash = 3841599891, size = 163264, name = "Klonk" }
      , { hash = 603390517, size = 259264, name = "Later" }
      , { hash = 4130279547, size = 163264, name = "Mallet" }
      , { hash = 3611087011, size = 46504, name = "Mellow" }
      , { hash = 1382847441, size = 225664, name = "Middle" }
      , { hash = 1911601043, size = 195756, name = "Mouth" }
      , { hash = 1976104117, size = 237346, name = "Noice" }
      , { hash = 2580359471, size = 110464, name = "One" }
      , { hash = 712335667, size = 248324, name = "Phasor" }
      , { hash = 3281734577, size = 62554, name = "Plonk" }
      , { hash = 3789625577, size = 76864, name = "Quick" }
      , { hash = 3784984255, size = 172864, name = "Raver" }
      , { hash = 4276416023, size = 172864, name = "Ringer" }
      , { hash = 2932749719, size = 195638, name = "Rips" }
      , { hash = 3964492385, size = 345664, name = "Rubber" }
      , { hash = 1224351141, size = 43380, name = "Short" }
      , { hash = 1204964399, size = 363170, name = "Small" }
      , { hash = 2788735495, size = 36434, name = "Stout" }
      , { hash = 2750533317, size = 96922, name = "Subtle" }
      , { hash = 440583049, size = 245220, name = "Synced" }
      , { hash = 3322301467, size = 174964, name = "Trapper" }
      ]
    )

  , ( "/factory/Synths/Stabs & Pads"
    , [ { hash = 3290855319, size = 1570492, name = "Alchemistic" }
      , { hash = 2791079433, size = 1060808, name = "Badlands" }
      , { hash = 2301652721, size = 736216, name = "Bounce" }
      , { hash = 1571965091, size = 552914, name = "Bow" }
      , { hash = 1472550181, size = 578212, name = "Breather" }
      , { hash = 4281766695, size = 993270, name = "Breeze" }
      , { hash = 3571237717, size = 1198762, name = "Chillin" }
      , { hash = 4026280497, size = 1277426, name = "Chopped" }
      , { hash = 1458554725, size = 443584, name = "Clave" }
      , { hash = 4115439315, size = 244388, name = "Deep" }
      , { hash = 4057260839, size = 365088, name = "Disco" }
      , { hash = 2920252495, size = 475662, name = "Dragged" }
      , { hash = 150255725, size = 774258, name = "Drifter" }
      , { hash = 4216784643, size = 364154, name = "Droops" }
      , { hash = 63098675, size = 1162716, name = "Entrance" }
      , { hash = 42037827, size = 761538, name = "Fashionable" }
      , { hash = 3229214637, size = 637434, name = "Fried" }
      , { hash = 1644838621, size = 414630, name = "Glow" }
      , { hash = 1763656243, size = 676230, name = "Heatvision" }
      , { hash = 4168905955, size = 1232422, name = "Highway" }
      , { hash = 4201444237, size = 317572, name = "Hoover" }
      , { hash = 2723582015, size = 384162, name = "Icy" }
      , { hash = 443215897, size = 728994, name = "Judgement" }
      , { hash = 3113606781, size = 346688, name = "Keys" }
      , { hash = 161431979, size = 177754, name = "Kicks" }
      , { hash = 3939930923, size = 463344, name = "Kiss" }
      , { hash = 4196386445, size = 314502, name = "Legit" }
      , { hash = 253143429, size = 593908, name = "Lowkey" }
      , { hash = 3784590805, size = 958112, name = "Opener" }
      , { hash = 3384748687, size = 180824, name = "Pick" }
      , { hash = 3388636511, size = 2326294, name = "Plato" }
      , { hash = 185306287, size = 358742, name = "Pluck" }
      , { hash = 2739520721, size = 192746, name = "Ripper" }
      , { hash = 1392803545, size = 1119424, name = "Skip" }
      , { hash = 2127656117, size = 336096, name = "Slower" }
      , { hash = 39823415, size = 40510, name = "Stabby" }
      , { hash = 879730295, size = 367226, name = "Strung" }
      , { hash = 1490147483, size = 424414, name = "Tennis" }
      , { hash = 327036493, size = 717392, name = "Thoughtful" }
      , { hash = 3868600511, size = 701740, name = "Tubular" }
      , { hash = 569815659, size = 195202, name = "Typical" }
      , { hash = 1933413499, size = 1277722, name = "UFO" }
      , { hash = 3897813221, size = 1577218, name = "VCR" }
      , { hash = 3633195799, size = 405976, name = "Whisp" }
      ]
    )

  , ( "/factory/Toolbox/Noise"
    , [ { hash = 4039745321, size = 168594, name = "Bassy" }
      , { hash = 2622830837, size = 114422, name = "Browny" }
      , { hash = 3401550977, size = 212988, name = "Empty" }
      , { hash = 2891845415, size = 136228, name = "Glassy" }
      , { hash = 125436621, size = 185986, name = "Grainy" }
      , { hash = 2287474367, size = 150762, name = "Pillowy" }
      , { hash = 419550331, size = 93468, name = "Rainy" }
      , { hash = 3250378627, size = 122778, name = "Snowy" }
      , { hash = 1386758245, size = 111842, name = "Staticy" }
      , { hash = 1916836939, size = 229162, name = "Subtly" }
      , { hash = 1413159997, size = 115592, name = "Velvety" }
      , { hash = 218569917, size = 63104, name = "Watery" }
      ]
    )

  , ( "/factory/Toolbox/Oscillators"
    , [ { hash = 1682276933, size = 798, name = "Acid Org" }
      , { hash = 2589330387, size = 798, name = "Acid Pul" }
      , { hash = 3061034183, size = 798, name = "Acid Pul 01" }
      , { hash = 2658197569, size = 798, name = "Acid Pul 02" }
      , { hash = 417301863, size = 430, name = "Acid Res" }
      , { hash = 1765186877, size = 798, name = "Acid Saw" }
      , { hash = 609430587, size = 798, name = "Acid Squ" }
      , { hash = 3617696827, size = 1532, name = "Acid Sub 01" }
      , { hash = 1684565289, size = 1534, name = "Acid Sub 02" }
      , { hash = 1370267899, size = 1526, name = "Acid Sub 03" }
      , { hash = 1047521157, size = 1526, name = "Acid Sub 04" }
      , { hash = 1253220473, size = 434, name = "Amiga" }
      , { hash = 433468097, size = 434, name = "Amigo" }
      , { hash = 3611061299, size = 800, name = "Anasine" }
      , { hash = 2170909281, size = 432, name = "Asqu" }
      , { hash = 2020499967, size = 430, name = "Brite" }
      , { hash = 2298236823, size = 1530, name = "Cave" }
      , { hash = 3374734501, size = 800, name = "Classic" }
      , { hash = 2396923137, size = 430, name = "Clav" }
      , { hash = 809314885, size = 800, name = "Comb" }
      , { hash = 2821416049, size = 794, name = "Curly" }
      , { hash = 1744188503, size = 800, name = "Dblfold" }
      , { hash = 3130345829, size = 802, name = "Deep" }
      , { hash = 1841265523, size = 804, name = "Donk" }
      , { hash = 2792695551, size = 432, name = "FM 01" }
      , { hash = 4179441521, size = 798, name = "FM 02" }
      , { hash = 2545299737, size = 430, name = "FM 03" }
      , { hash = 548688401, size = 430, name = "FM 04" }
      , { hash = 3436309271, size = 432, name = "FM 05" }
      , { hash = 4252463139, size = 430, name = "FM 06" }
      , { hash = 2501376673, size = 798, name = "FM 07" }
      , { hash = 1021797761, size = 798, name = "FM 08" }
      , { hash = 646380409, size = 430, name = "FM 09" }
      , { hash = 1948987125, size = 798, name = "FM 10" }
      , { hash = 2809314249, size = 798, name = "FM 11" }
      , { hash = 4071777009, size = 800, name = "FMish" }
      , { hash = 3421378945, size = 1554, name = "FMsqu" }
      , { hash = 403932073, size = 798, name = "Folded 01" }
      , { hash = 269898559, size = 798, name = "Folded 02" }
      , { hash = 1844019357, size = 798, name = "Folded 03" }
      , { hash = 3150129045, size = 796, name = "Folded 04" }
      , { hash = 3940523193, size = 798, name = "Folded 05" }
      , { hash = 421498827, size = 798, name = "Folded 06" }
      , { hash = 89549369, size = 798, name = "Gentle" }
      , { hash = 1586199597, size = 796, name = "Holloramp" }
      , { hash = 3609746765, size = 804, name = "Hollosharp" }
      , { hash = 273365721, size = 804, name = "Hollow" }
      , { hash = 2292988371, size = 798, name = "Less" }
      , { hash = 2192517691, size = 798, name = "Low" }
      , { hash = 2760088289, size = 798, name = "Nasal" }
      , { hash = 1138773239, size = 798, name = "Oboe" }
      , { hash = 592978639, size = 430, name = "Organ 01" }
      , { hash = 3792407345, size = 800, name = "Organ 02" }
      , { hash = 3057632739, size = 430, name = "Poly" }
      , { hash = 3708392947, size = 800, name = "Reed" }
      , { hash = 4113958331, size = 430, name = "Rekt" }
      , { hash = 3364325211, size = 798, name = "Saw" }
      , { hash = 742777099, size = 798, name = "Sawfold" }
      , { hash = 363196881, size = 804, name = "Sharp" }
      , { hash = 644400091, size = 804, name = "Sharptri" }
      , { hash = 1118264893, size = 798, name = "Sides" }
      , { hash = 2064812713, size = 798, name = "Sine" }
      , { hash = 3540098021, size = 434, name = "Skev" }
      , { hash = 1053183537, size = 796, name = "Skewed" }
      , { hash = 542598441, size = 806, name = "Spikes" }
      , { hash = 1219499791, size = 796, name = "Subosc" }
      , { hash = 375934821, size = 798, name = "Thick" }
      , { hash = 3860939657, size = 798, name = "Thin" }
      , { hash = 1416713059, size = 804, name = "Tones" }
      , { hash = 2739365977, size = 796, name = "Tri" }
      , { hash = 2822989779, size = 432, name = "Trifold 01" }
      , { hash = 28735643, size = 804, name = "Trifold 02" }
      ]
    )

  , ( "/factory/Drums/Acoustic/Ambient Kit"
    , [ { hash = 2050951153, size = 322580, name = "CP Ambient" }
      , { hash = 2823541499, size = 670880, name = "CY Ambient" }
      , { hash = 714150573, size = 77842, name = "HH Ambient" }
      , { hash = 2325119539, size = 165280, name = "HT Ambient" }
      , { hash = 3284752375, size = 185026, name = "LT Ambient" }
      , { hash = 2751568889, size = 223368, name = "MT Ambient" }
      , { hash = 2365302761, size = 279854, name = "SD Ambient" }
      , { hash = 3115627571, size = 352468, name = "SP Ambient" }
      ]
    )

  , ( "/factory/Drums/Acoustic/Dirty Kit"
    , [ { hash = 1889368597, size = 148864, name = "BD1 Dirty" }
      , { hash = 3286921735, size = 134464, name = "BD2 Dirty" }
      , { hash = 2191653637, size = 114600, name = "CP Dirty" }
      , { hash = 2258613883, size = 889358, name = "CY Dirty" }
      , { hash = 1673996417, size = 4864, name = "HH Dirty" }
      , { hash = 3443423789, size = 225944, name = "LT Dirty" }
      , { hash = 407154483, size = 168064, name = "MT Dirty" }
      , { hash = 1771213557, size = 172864, name = "SD Dirty" }
      ]
    )

  , ( "/factory/Drums/Acoustic/Natural Kit"
    , [ { hash = 3562521245, size = 163968, name = "BD Natural" }
      , { hash = 837916901, size = 537664, name = "CY Natural" }
      , { hash = 3466689467, size = 102718, name = "HH Natural" }
      , { hash = 2798978455, size = 190922, name = "LT Natural" }
      , { hash = 3656039203, size = 153664, name = "MT Natural" }
      , { hash = 3011210385, size = 785994, name = "RD Natural" }
      , { hash = 1326871377, size = 133836, name = "SD Natural" }
      ]
    )

  , ( "/factory/Drums/Acoustic/Punchy Kit"
    , [ { hash = 3712641913, size = 107036, name = "BD Punchy" }
      , { hash = 3379742077, size = 268960, name = "BT Punchy" }
      , { hash = 2866172509, size = 138444, name = "CP Punchy" }
      , { hash = 4067554305, size = 945726, name = "CY Punchy" }
      , { hash = 2046517889, size = 34624, name = "HH Punchy" }
      , { hash = 750814917, size = 154484, name = "HT Punchy" }
      , { hash = 3073566611, size = 213878, name = "LT Punchy" }
      , { hash = 2233642341, size = 233108, name = "MT Punchy" }
      ]
    )

  , ( "/factory/Drums/Acoustic/Slapped Perc"
    , [ { hash = 1915578961, size = 802398, name = "Bendir 1" }
      , { hash = 1264641105, size = 122610, name = "Bendir 2" }
      , { hash = 40714909, size = 284476, name = "Bendir 3" }
      , { hash = 2834264035, size = 353376, name = "Bendir 4" }
      , { hash = 3992904813, size = 294566, name = "Darbuka Clay Dum 1" }
      , { hash = 3062624705, size = 149202, name = "Darbuka Clay Dum 2" }
      , { hash = 3755101245, size = 51522, name = "Darbuka Clay Edge" }
      , { hash = 2747972095, size = 45114, name = "Darbuka Clay Slap" }
      , { hash = 1119130331, size = 56938, name = "Darbuka Clay Slide" }
      , { hash = 76153369, size = 157332, name = "Darbuka Clay Snare 1" }
      , { hash = 2881845251, size = 112534, name = "Darbuka Clay Snare 2" }
      , { hash = 1991427695, size = 37588, name = "Darbuka Metal 1" }
      , { hash = 315126721, size = 14392, name = "Darbuka Metal 2" }
      , { hash = 735793217, size = 105362, name = "Darbuka Metal 3" }
      , { hash = 3187622159, size = 64284, name = "Darbuka Metal 4" }
      , { hash = 321514109, size = 41328, name = "Finger Cymbal" }
      , { hash = 1101830089, size = 358224, name = "Steinn Noisy" }
      , { hash = 2855542173, size = 229616, name = "Steinn Rim" }
      , { hash = 2001566777, size = 277878, name = "Steinn Snare" }
      , { hash = 3114268589, size = 220002, name = "Tambourine 1" }
      , { hash = 307422735, size = 81398, name = "Tambourine 2" }
      , { hash = 4132308459, size = 38128, name = "Tambourine 3" }
      , { hash = 3560641805, size = 76732, name = "Tambourine 4" }
      , { hash = 912332963, size = 55790, name = "Tambourine 5" }
      , { hash = 2376574831, size = 92814, name = "Tombak Bass" }
      , { hash = 3465853963, size = 75370, name = "Tombak Double" }
      , { hash = 4085146651, size = 27612, name = "Tombak Edge" }
      , { hash = 2082981623, size = 44786, name = "Tombak Fingers" }
      , { hash = 3830147603, size = 65706, name = "Tombak Knock" }
      , { hash = 3554105601, size = 69102, name = "Tombak Rim" }
      , { hash = 1336960345, size = 85202, name = "Tombak Snare" }
      , { hash = 2222839691, size = 80008, name = "Tombak Tom" }
      ]
    )

  , ( "/factory/Drums/Electronic/Abrasive"
    , [ { hash = 453629625, size = 96058, name = "BD1 Abrasive" }
      , { hash = 3960220237, size = 96058, name = "BD2 Abrasive" }
      , { hash = 1721717317, size = 120064, name = "CB1 Abrasive" }
      , { hash = 3783147935, size = 186526, name = "CB2 Abrasive" }
      , { hash = 496756547, size = 144064, name = "MT1 Abrasive" }
      , { hash = 1315514127, size = 90528, name = "MT2 Abrasive" }
      , { hash = 1893069493, size = 114932, name = "MT3 Abrasive" }
      , { hash = 1780918391, size = 42498, name = "PC1 Abrasive" }
      , { hash = 3126209245, size = 90348, name = "PC2 Abrasive" }
      , { hash = 2687881419, size = 186524, name = "PC3 Abrasive" }
      , { hash = 2429442999, size = 114504, name = "PC4 Abrasive" }
      , { hash = 2202246649, size = 90504, name = "SD1 Abrasive" }
      , { hash = 4146646443, size = 89498, name = "SD2 Abrasive" }
      , { hash = 3242562037, size = 90520, name = "SD3 Abrasive" }
      , { hash = 2769963285, size = 138522, name = "SD4 Abrasive" }
      , { hash = 793697517, size = 42522, name = "SD5 Abrasive" }
      ]
    )

  , ( "/factory/Drums/Electronic/Adlib"
    , [ { hash = 2775936515, size = 74610, name = "BD Adlib" }
      , { hash = 3772289645, size = 29304, name = "CB Adlib" }
      , { hash = 948130245, size = 4566, name = "CL Adlib" }
      , { hash = 2958702495, size = 245112, name = "CY Adlib" }
      , { hash = 1272073153, size = 21270, name = "HH Adlib" }
      , { hash = 246116297, size = 56684, name = "MT Adlib" }
      , { hash = 863475037, size = 4146, name = "RS Adlib" }
      , { hash = 3879522185, size = 55528, name = "SD Adlib" }
      ]
    )

  , ( "/factory/Drums/Electronic/Booming"
    , [ { hash = 1053062609, size = 192064, name = "BD Booming" }
      , { hash = 3789103107, size = 42512, name = "CL Booming" }
      , { hash = 145937297, size = 90510, name = "CP Booming" }
      , { hash = 2762217321, size = 45684, name = "HH Booming" }
      , { hash = 2533447415, size = 146444, name = "MT Booming" }
      , { hash = 1950650969, size = 93580, name = "OH Booming" }
      , { hash = 1277419129, size = 42502, name = "RS Booming" }
      , { hash = 3059234655, size = 42516, name = "SD Booming" }
      ]
    )

  , ( "/factory/Drums/Electronic/Bronze"
    , [ { hash = 3622389989, size = 123200, name = "BD1 Bronze" }
      , { hash = 1458481175, size = 84698, name = "BD2 Bronze" }
      , { hash = 269062041, size = 123542, name = "CP Bronze" }
      , { hash = 3092479575, size = 208406, name = "CY Bronze" }
      , { hash = 1423971365, size = 61054, name = "HH Bronze" }
      , { hash = 1707867849, size = 143474, name = "MT Bronze" }
      , { hash = 726283775, size = 101260, name = "RS Bronze" }
      , { hash = 3562143453, size = 122854, name = "ST Bronze" }
      ]
    )

  , ( "/factory/Drums/Electronic/Buggin"
    , [ { hash = 44887955, size = 36062, name = "BD1 Buggin" }
      , { hash = 174180907, size = 108062, name = "BD2 Buggin" }
      , { hash = 4023800719, size = 59286, name = "HH1 Buggin" }
      , { hash = 1797563723, size = 56734, name = "HH2 Buggin" }
      , { hash = 4261459963, size = 98596, name = "MT Buggin" }
      , { hash = 1476154435, size = 46750, name = "RS Buggin" }
      , { hash = 2331312373, size = 45470, name = "SD1 Buggin" }
      , { hash = 2311616311, size = 45594, name = "SD2 Buggin" }
      ]
    )

  , ( "/factory/Drums/Electronic/Cable"
    , [ { hash = 632705171, size = 144062, name = "BD Cable" }
      , { hash = 1893356817, size = 30528, name = "CA Cable" }
      , { hash = 4113689349, size = 39576, name = "HH Cable" }
      , { hash = 4075275119, size = 47390, name = "MT Cable" }
      , { hash = 4076747997, size = 57998, name = "OH Cable" }
      , { hash = 3181929769, size = 15134, name = "RS Cable" }
      , { hash = 846830385, size = 44704, name = "SD Cable" }
      ]
    )

  , ( "/factory/Drums/Electronic/Class"
    , [ { hash = 1378410779, size = 96048, name = "BD Class" }
      , { hash = 1051298389, size = 42110, name = "CB Class" }
      , { hash = 1139728051, size = 42112, name = "CL Class" }
      , { hash = 1199572459, size = 26308, name = "CP Class" }
      , { hash = 1213275125, size = 15068, name = "HH Class" }
      , { hash = 2755882515, size = 45818, name = "MT Class" }
      , { hash = 1540291755, size = 66526, name = "OH Class" }
      , { hash = 2099150655, size = 42542, name = "SD Class" }
      ]
    )

  , ( "/factory/Drums/Electronic/Digit"
    , [ { hash = 1139277629, size = 144064, name = "BD Digit" }
      , { hash = 3996165251, size = 42530, name = "CB Digit" }
      , { hash = 2368422339, size = 33192, name = "CP Digit" }
      , { hash = 1187120671, size = 204952, name = "CY Digit" }
      , { hash = 1809077313, size = 54520, name = "HH Digit" }
      , { hash = 2276121603, size = 62368, name = "MT Digit" }
      , { hash = 2165069335, size = 72514, name = "OH Digit" }
      , { hash = 348138077, size = 45082, name = "SD Digit" }
      ]
    )

  , ( "/factory/Drums/Electronic/Don"
    , [ { hash = 1527052849, size = 191914, name = "BT Don" }
      , { hash = 3794808131, size = 109518, name = "CP Don" }
      , { hash = 3498187173, size = 43568, name = "HH1 Don" }
      , { hash = 3937773429, size = 43848, name = "HH2 Don" }
      , { hash = 1645256439, size = 151474, name = "HT Don" }
      , { hash = 2078484653, size = 155152, name = "LT Don" }
      , { hash = 2175702251, size = 125890, name = "MT Don" }
      , { hash = 1972745265, size = 188684, name = "NS1 Don" }
      , { hash = 3597379659, size = 174932, name = "NS2 Don" }
      , { hash = 2093133419, size = 94456, name = "RS Don" }
      , { hash = 3327397857, size = 113252, name = "SD1 Don" }
      , { hash = 3913926295, size = 140876, name = "SD2 Don" }
      , { hash = 712963073, size = 191898, name = "SD3 Don" }
      ]
    )

  , ( "/factory/Drums/Electronic/Fella"
    , [ { hash = 2755333529, size = 107966, name = "BD1 Fella" }
      , { hash = 4060939691, size = 281410, name = "BD2 Fella" }
      , { hash = 1085669151, size = 98202, name = "BD3 Fella" }
      , { hash = 4279226129, size = 262720, name = "BD4 Fella" }
      , { hash = 1718001001, size = 17570, name = "CP Fella" }
      , { hash = 2886884383, size = 257188, name = "CY Fella" }
      , { hash = 423582397, size = 145062, name = "HH Fella" }
      , { hash = 2234964977, size = 145062, name = "HH2 Fella" }
      , { hash = 211582373, size = 103038, name = "MT Fella" }
      , { hash = 2654504729, size = 104098, name = "OH Fella" }
      , { hash = 4149900063, size = 180062, name = "OH2 Fella" }
      , { hash = 3609738979, size = 84802, name = "PC1 Fella" }
      , { hash = 1948795943, size = 131738, name = "PC2 Fella" }
      , { hash = 2757327865, size = 135810, name = "PC3 Fella" }
      , { hash = 3299472905, size = 25630, name = "RS Fella" }
      , { hash = 3456512597, size = 80274, name = "SD1 Fella" }
      , { hash = 2635909905, size = 74390, name = "SD2 Fella" }
      , { hash = 1352455847, size = 51362, name = "SD3 Fella" }
      , { hash = 3797722685, size = 45430, name = "SD4 Fella" }
      , { hash = 1433464391, size = 168096, name = "SP Fella" }
      ]
    )

  , ( "/factory/Drums/Electronic/Flange"
    , [ { hash = 732887511, size = 102424, name = "BD1 Flange" }
      , { hash = 1267711783, size = 166938, name = "BD2 Flange" }
      , { hash = 610668523, size = 158744, name = "BD3 Flange" }
      , { hash = 2090756357, size = 101652, name = "BD4 Flange" }
      , { hash = 2106932801, size = 277118, name = "CY Flange" }
      , { hash = 675521171, size = 138808, name = "HH1 Flange" }
      , { hash = 2750077833, size = 247428, name = "HH2 Flange" }
      , { hash = 1740017081, size = 157312, name = "HH3 Flange" }
      , { hash = 803738087, size = 219260, name = "MT Flange" }
      , { hash = 2950757065, size = 221312, name = "PC1 Flange" }
      , { hash = 3963211645, size = 251000, name = "PC2 Flange" }
      , { hash = 951698201, size = 205438, name = "PC3 Flange" }
      , { hash = 293573243, size = 126076, name = "SD1 Flange" }
      , { hash = 758389503, size = 124026, name = "SD2 Flange" }
      , { hash = 2903291537, size = 188540, name = "SD3 Flange" }
      , { hash = 463948859, size = 188028, name = "SD4 Flange" }
      ]
    )

  , ( "/factory/Drums/Electronic/Pearl"
    , [ { hash = 3994081591, size = 96064, name = "BD Pearl" }
      , { hash = 3127917223, size = 236226, name = "CB Pearl" }
      , { hash = 2894119139, size = 20064, name = "CL Pearl" }
      , { hash = 1329288469, size = 34478, name = "LT Pearl" }
      , { hash = 4022162503, size = 50064, name = "PC Pearl" }
      , { hash = 2579147773, size = 10590, name = "RS Pearl" }
      , { hash = 1566049591, size = 20064, name = "SD Pearl" }
      , { hash = 3423792885, size = 188058, name = "SY Pearl" }
      ]
    )

  , ( "/factory/Drums/Electronic/Puff"
    , [ { hash = 1783618171, size = 23424, name = "BD1 Puff" }
      , { hash = 3170911575, size = 192058, name = "BD2 Puff" }
      , { hash = 3129314493, size = 21560, name = "CB Puff" }
      , { hash = 3255091671, size = 16480, name = "CL Puff" }
      , { hash = 359674347, size = 66522, name = "HH Puff" }
      , { hash = 713601405, size = 44694, name = "HT Puff" }
      , { hash = 461843749, size = 45468, name = "MT Puff" }
      , { hash = 3510175877, size = 45988, name = "SD Puff" }
      ]
    )

  , ( "/factory/Drums/Electronic/Reson"
    , [ { hash = 3424937007, size = 61726, name = "BD Reson" }
      , { hash = 1016625579, size = 58786, name = "HH Reson" }
      , { hash = 4012954605, size = 142712, name = "MT Reson" }
      , { hash = 523044043, size = 184978, name = "OH Reson" }
      , { hash = 633015141, size = 165536, name = "PC Reson" }
      , { hash = 3711235203, size = 80544, name = "RS Reson" }
      , { hash = 340312669, size = 101022, name = "SD1 Reson" }
      , { hash = 1972269489, size = 38810, name = "SD2 Reson" }
      ]
    )

  , ( "/factory/Drums/Electronic/Rocking"
    , [ { hash = 1602387793, size = 75964, name = "BD Rocking" }
      , { hash = 2417978701, size = 9160, name = "CB Rocking" }
      , { hash = 4029245541, size = 51724, name = "CP Rocking" }
      , { hash = 2681454377, size = 191998, name = "CY Rocking" }
      , { hash = 1958464499, size = 23608, name = "HH Rocking" }
      , { hash = 302632397, size = 87382, name = "HT Rocking" }
      , { hash = 4258241149, size = 92678, name = "LT Rocking" }
      , { hash = 222327243, size = 79404, name = "SD Rocking" }
      ]
    )

  , ( "/factory/Drums/Electronic/Rom"
    , [ { hash = 1962983837, size = 60062, name = "BD Rom" }
      , { hash = 3759519671, size = 192168, name = "CY Rom" }
      , { hash = 2830362537, size = 45734, name = "HH Rom" }
      , { hash = 2623687467, size = 54540, name = "MT Rom" }
      , { hash = 3664353819, size = 96932, name = "OH Rom" }
      , { hash = 516420319, size = 141606, name = "RD Rom" }
      , { hash = 690505565, size = 26060, name = "RS Rom" }
      , { hash = 3644178745, size = 46764, name = "SD Rom" }
      ]
    )

  , ( "/factory/Drums/Electronic/School"
    , [ { hash = 2824609755, size = 34542, name = "BD School" }
      , { hash = 563098601, size = 14010, name = "HH1 School" }
      , { hash = 584846149, size = 25758, name = "HH2 School" }
      , { hash = 2950770455, size = 19296, name = "OH School" }
      , { hash = 2803681789, size = 48004, name = "PC1 School" }
      , { hash = 729026127, size = 24994, name = "PC2 School" }
      , { hash = 1135720051, size = 20630, name = "RS School" }
      , { hash = 1240809129, size = 49064, name = "SD School" }
      ]
    )

  , ( "/factory/Drums/Electronic/Soft"
    , [ { hash = 3322801105, size = 72056, name = "BD Soft" }
      , { hash = 40327539, size = 43680, name = "CB Soft" }
      , { hash = 1852776721, size = 11224, name = "CL Soft" }
      , { hash = 718209921, size = 141434, name = "CY Soft" }
      , { hash = 2933206711, size = 70294, name = "HH Soft" }
      , { hash = 3955188807, size = 90510, name = "MT Soft" }
      , { hash = 260379633, size = 27920, name = "RS Soft" }
      , { hash = 2750506331, size = 90534, name = "SD Soft" }
      ]
    )

  , ( "/factory/Drums/Electronic/Spring"
    , [ { hash = 2168370381, size = 192064, name = "BD Spring" }
      , { hash = 422746755, size = 95402, name = "CL Spring" }
      , { hash = 686550177, size = 345836, name = "PC1 Spring" }
      , { hash = 3958828027, size = 389092, name = "PC2 Spring" }
      , { hash = 746356413, size = 192170, name = "SD1 Spring" }
      , { hash = 959451841, size = 289448, name = "SD2 Spring" }
      , { hash = 2352742687, size = 332494, name = "SY1 Spring" }
      , { hash = 2349030453, size = 196184, name = "SY2 Spring" }
      ]
    )

  , ( "/factory/Drums/Electronic/Stonk"
    , [ { hash = 889809715, size = 87600, name = "BD Stonk" }
      , { hash = 3044515919, size = 88210, name = "CB Stonk" }
      , { hash = 144274987, size = 132768, name = "HH Stonk" }
      , { hash = 760282299, size = 138744, name = "HT Stonk" }
      , { hash = 4218933827, size = 138874, name = "MT Stonk" }
      , { hash = 1812466605, size = 25662, name = "RS Stonk" }
      , { hash = 1643468379, size = 42662, name = "SD Stonk" }
      ]
    )

  , ( "/factory/Drums/Electronic/Trunk"
    , [ { hash = 1006065941, size = 88586, name = "BD Trunk" }
      , { hash = 905539497, size = 40334, name = "CP Trunk" }
      , { hash = 2426931503, size = 86424, name = "MC Trunk" }
      , { hash = 2303669155, size = 173770, name = "OH Trunk" }
      , { hash = 1480870903, size = 130684, name = "PC Trunk" }
      , { hash = 1473935865, size = 15204, name = "RS Trunkl" }
      , { hash = 2843490465, size = 41638, name = "SD Trunk" }
      ]
    )

  , ( "/factory/Drums/Electronic/Velvet"
    , [ { hash = 1715372189, size = 57664, name = "BD Velvet" }
      , { hash = 1472057673, size = 45566, name = "CB Velvet" }
      , { hash = 2399537585, size = 52136, name = "CP Velvet" }
      , { hash = 1750675801, size = 115320, name = "HH1 Velvet" }
      , { hash = 38392563, size = 82590, name = "HH2 Velvet" }
      , { hash = 1990961525, size = 305828, name = "LT Velvet" }
      , { hash = 3114111797, size = 114978, name = "MT Velvet" }
      , { hash = 1594888231, size = 55972, name = "RS Velvet" }
      , { hash = 2621616677, size = 38570, name = "SD Velvet" }
      ]
    )

  , ( "/factory/Drums/Electronic/Weird"
    , [ { hash = 3925089181, size = 96064, name = "BD1 Weird" }
      , { hash = 1339113459, size = 48064, name = "BD2 Weird" }
      , { hash = 3143110323, size = 42524, name = "CP Weird" }
      , { hash = 3089904961, size = 48538, name = "HH1 Weird" }
      , { hash = 3603257391, size = 45734, name = "HH2 Weird" }
      , { hash = 2254360247, size = 23124, name = "RS Weird" }
      , { hash = 2143599789, size = 42482, name = "SD1 Weird" }
      , { hash = 2182760157, size = 54592, name = "SD2 Weird" }
      ]
    )

  ]
