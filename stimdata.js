lex_dict = {
    // 'en': [
    //     { 'word': 'platery', 'wstatus': 0, 'dummy': 1 },
    //     { 'word': 'denial', 'wstatus': 1, 'dummy': 1 },
    //     { 'word': 'generic', 'wstatus': 1, 'dummy': 1 },
    //     { 'word': 'mensible', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'scornful', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'stoutly', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'ablaze', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'kermshaw', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'moonlit', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'lofty', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'hurricane', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'flaw', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'alberation', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'unkempt', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'breeding', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'festivity', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'screech', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'savoury', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'plaudate', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'shin', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'fluid', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'spaunch', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'allied', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'slain', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'recipient', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'exprate', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'eloquence', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'cleanliness', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'dispatch', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'rebondicate', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'ingenious', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'bewitch', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'skave', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'plaintively', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'kilp', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'interfate', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'hasty', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'lengthy', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'fray', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'crumper', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'upkeep', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'majestic', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'magrity', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'nourishment', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'abergy', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'proom', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'turmoil', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'carbohydrate', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'scholar', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'turtle', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'fellick', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'destription', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'cylinder', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'censorship', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'celestial', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'rascal', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'purrage', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'pulsh', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'muddy', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'quirty', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'pudour', 'wstatus': 0, 'dummy': 0 },
    //     { 'word': 'listless', 'wstatus': 1, 'dummy': 0 },
    //     { 'word': 'wrought', 'wstatus': 1, 'dummy': 0 }
    // ],
    // 'de': [
    //     { 'word': "WOLLE", 'wstatus': 1, 'dummy': 1 },
    //     { 'word': "TERMITÄT", 'wstatus': 0, 'dummy': 1 },
    //     { 'word': "ENORM", 'wstatus': 1, 'dummy': 1 },
    //     { 'word': "WELSTBAR", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "REUEVOLL", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "ZUOBERST", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "RUPPIG", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "PETURAT", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "KLAGLOS", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "ZUGIG", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "TURBULENZ", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "ZEHE", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "DEGERATION", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "UNTIEF", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "ZÜCHTUNG", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "PENSIONAT", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "ZAPFEN", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "STAKSIG", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "STALMEN", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "MALZ", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "FEIGE", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "FÜRREN", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "RASEND", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "FEIST", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "KLEMPNER", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "AUSREBEN", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "KANNIBALE", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "SCHWACHHEIT", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "ERBARMEN", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "VERMASTIGEN", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "SUBVERSIV", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "SATTELN", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "PLANG", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "EIMERWEISE", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "SCHEIL", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "STOCKFEST", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "MEHLIG", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "DÄMMRIG", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "GAREN", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "TRACHTER", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "ANPROBE", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "MONSTRÖS", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "SONITÄT", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "SPEICHERUNG", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "MALODIE", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "NARKE", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "STRÄHNE", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "DESTILLATION", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "LEUCHTE", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "FLINTE", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "MACKEL", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "ENTSACHTUNG", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "GEOGRAPH", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "SUMMIERUNG", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "WAGHALSIG", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "ZIERDE", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "FAUNIK", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "LUDAL", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "KLAMM", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "DRAUNIG", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "FLISTOR", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "UNSTETIG", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "HERZIG", 'wstatus': 1, 'dummy': 0 }
    //     ],
    // 'nl': [
    //     { 'word': "pastitie", 'wstatus': 0, 'dummy': 1 },
    //     { 'word': "scheur", 'wstatus': 1, 'dummy': 1 },
    //     { 'word': "fobisch", 'wstatus': 1, 'dummy': 1 },
    //     { 'word': "markatief", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "laakbaar", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "slaags", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "riant", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "joutbaag", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "doornat", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "woelig", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "paviljoen", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "doop", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "starkatie", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "onledig", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "toetsing", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "affiniteit", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "mikken", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "knullig", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "streuren", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "rups", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "paars", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "speven", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "geraakt", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "martelaar", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "ontpelen", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "stagnatie", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "dronkenschap", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "voornemen", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "vertediseren", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "normatief", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "zetelen", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "zolf", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "publiekelijk", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "vluk", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "compromeet", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "romig", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "getint", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "gelovig", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "nopen", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "kluiper", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "geloei", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "retorisch", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "maliteit", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "verspilling", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "haperie", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "proom", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "fornuis", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "exploitatie", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "acteur", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "hengel", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "flajoen", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "aanhekking", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "kazerne", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "avonturier", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "leurig", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "chagrijnig", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "bretel", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "klengel", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "etaal", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "matig", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "futeur", 'wstatus': 0, 'dummy': 0 },
    //     { 'word': "onbekwaam", 'wstatus': 1, 'dummy': 0 },
    //     { 'word': "verguld", 'wstatus': 1, 'dummy': 0 }
    // ],
    'ch': [
        { 'filename': 'LEXTALE_CH_lexitem148.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem008.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem159.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem086.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem136.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem094.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem003.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem180.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem145.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem093.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem019.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_nonceitem015.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem170.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem150.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem125.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem116.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem128.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem178.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem049.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem107.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem115.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem017.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem102.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem168.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem004.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem139.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem175.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem043.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_nonceitem039.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem109.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem156.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem173.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem172.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem022.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem118.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem027.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem157.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem007.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem165.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem108.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem154.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem137.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem097.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem133.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem023.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem179.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem155.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem062.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem105.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem089.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_nonceitem024.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem144.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem143.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem044.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_nonceitem040.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem100.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem162.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem171.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem026.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem119.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem101.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem176.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem098.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem016.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_nonceitem013.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem153.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem147.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem068.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem177.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem140.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem112.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem114.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem161.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem029.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_nonceitem034.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem121.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem095.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem059.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem129.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem047.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem106.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem103.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem037.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem127.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem158.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem167.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem005.png', 'valid': 0 },
        { 'filename': 'LEXTALE_CH_lexitem130.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_lexitem138.png', 'valid': 1 },
        { 'filename': 'LEXTALE_CH_nonceitem042.png', 'valid': 0 }
    ]
};