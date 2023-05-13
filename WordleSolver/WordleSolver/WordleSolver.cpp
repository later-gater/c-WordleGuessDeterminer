// WordleSolver.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "WordleSolver.h"
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <numeric>



struct letterStruct {
    char letter;
    bool gray = false;
    int numLetters = 0; // if this is 0, we have n
    std::vector<int> greens;
    std::vector<int> yellows;
    void printMe()
    {
        std::cout << letter << " properties:\n" << "\t - gray: " << gray << "\n\t - numLetters: " << numLetters << "\n\t - greens: ";
        for (int green : greens)
        {
            std::cout << green << " ";
        }
        std::cout << "\n\t - yellows: ";
        for (int yellow : yellows)
        {
            std::cout << yellow << " ";
        }
        std::cout << "\n";
    }
};

std::vector<std::string> MasterList = { "cigar","rebut","sissy","humph","awake","blush","focal","evade","naval","serve","heath","dwarf","model","karma","stink","grade","quiet","bench","abate","feign","major","death","fresh","crust","stool","colon","abase","marry","react","batty","pride","floss","helix","croak","staff","paper","unfed","whelp","trawl","outdo","adobe","crazy","sower","repay","digit","crate","cluck","spike","mimic","pound","maxim","linen","unmet","flesh","forth","first","stand","belly","ivory","seedy","print","yearn","drain","bribe","stout","panel","crass","flume","offal","agree","error","swirl","argue","bleed","delta","flick","totem","wooer","front","shrub","parry","biome","lapel","start","greet","goner","golem","lusty","loopy","round","audit","lying","gamma","labor","islet","civic","forge","corny","moult","basic","salad","agate","spicy","spray","essay","fjord","spend","kebab","guild","aback","motor","alone","hatch","hyper","thumb","dowry","ought","belch","dutch","pilot","tweed","comet","jaunt","enema","steed","abyss","growl","fling","dozen","boozy","erode","world","gouge","click","briar","great","altar","pulpy","blurt","coast","duchy","groin","fixer","group","rogue","badly","smart","pithy","gaudy","chill","heron","vodka","finer","surer","radio","rouge","perch","retch","wrote","clock","tilde","store","prove","bring","solve","cheat","grime","exult","usher","epoch","triad","break","rhino","viral","conic","masse","sonic","vital","trace","using","peach","champ","baton","brake","pluck","craze","gripe","weary","picky","acute","ferry","aside","tapir","troll","unify","rebus","boost","truss","siege","tiger","banal","slump","crank","gorge","query","drink","favor","abbey","tangy","panic","solar","shire","proxy","point","robot","prick","wince","crimp","knoll","sugar","whack","mount","perky","could","wrung","light","those","moist","shard","pleat","aloft","skill","elder","frame","humor","pause","ulcer","ultra","robin","cynic","aroma","caulk","shake","dodge","swill","tacit","other","thorn","trove","bloke","vivid","spill","chant","choke","rupee","nasty","mourn","ahead","brine","cloth","hoard","sweet","month","lapse","watch","today","focus","smelt","tease","cater","movie","saute","allow","renew","their","slosh","purge","chest","depot","epoxy","nymph","found","shall","stove","lowly","snout","trope","fewer","shawl","natal","comma","foray","scare","stair","black","squad","royal","chunk","mince","shame","cheek","ample","flair","foyer","cargo","oxide","plant","olive","inert","askew","heist","shown","zesty","trash","larva","forgo","story","hairy","train","homer","badge","midst","canny","shine","gecko","farce","slung","tipsy","metal","yield","delve","being","scour","glass","gamer","scrap","money","hinge","album","vouch","asset","tiara","crept","bayou","atoll","manor","creak","showy","phase","froth","depth","gloom","flood","trait","girth","piety","goose","float","donor","atone","primo","apron","blown","cacao","loser","input","gloat","awful","brink","smite","beady","rusty","retro","droll","gawky","hutch","pinto","egret","lilac","sever","field","fluff","agape","voice","stead","berth","madam","night","bland","liver","wedge","roomy","wacky","flock","angry","trite","aphid","tryst","midge","power","elope","cinch","motto","stomp","upset","bluff","cramp","quart","coyly","youth","rhyme","buggy","alien","smear","unfit","patty","cling","glean","label","hunky","khaki","poker","gruel","twice","twang","shrug","treat","waste","merit","woven","needy","clown","irony","ruder","gauze","chief","onset","prize","fungi","charm","gully","inter","whoop","taunt","leery","class","theme","lofty","tibia","booze","alpha","thyme","doubt","parer","chute","stick","trice","alike","recap","saint","glory","grate","admit","brisk","soggy","usurp","scald","scorn","leave","twine","sting","bough","marsh","sloth","dandy","vigor","howdy","enjoy","valid","ionic","equal","floor","catch","spade","stein","exist","quirk","denim","grove","spiel","mummy","fault","foggy","flout","carry","sneak","libel","waltz","aptly","piney","inept","aloud","photo","dream","stale","unite","snarl","baker","there","glyph","pooch","hippy","spell","folly","louse","gulch","vault","godly","threw","fleet","grave","inane","shock","crave","spite","valve","skimp","claim","rainy","musty","pique","daddy","quasi","arise","aging","valet","opium","avert","stuck","recut","mulch","genre","plume","rifle","count","incur","total","wrest","mocha","deter","study","lover","safer","rivet","funny","smoke","mound","undue","sedan","pagan","swine","guile","gusty","equip","tough","canoe","chaos","covet","human","udder","lunch","blast","stray","manga","melee","lefty","quick","paste","given","octet","risen","groan","leaky","grind","carve","loose","sadly","spilt","apple","slack","honey","final","sheen","eerie","minty","slick","derby","wharf","spelt","coach","erupt","singe","price","spawn","fairy","jiffy","filmy","stack","chose","sleep","ardor","nanny","niece","woozy","handy","grace","ditto","stank","cream","usual","diode","valor","angle","ninja","muddy","chase","reply","prone","spoil","heart","shade","diner","arson","onion","sleet","dowel","couch","palsy","bowel","smile","evoke","creek","lance","eagle","idiot","siren","built","embed","award","dross","annul","goody","frown","patio","laden","humid","elite","lymph","edify","might","reset","visit","gusto","purse","vapor","crock","write","sunny","loath","chaff","slide","queer","venom","stamp","sorry","still","acorn","aping","pushy","tamer","hater","mania","awoke","brawn","swift","exile","birch","lucky","freer","risky","ghost","plier","lunar","winch","snare","nurse","house","borax","nicer","lurch","exalt","about","savvy","toxin","tunic","pried","inlay","chump","lanky","cress","eater","elude","cycle","kitty","boule","moron","tenet","place","lobby","plush","vigil","index","blink","clung","qualm","croup","clink","juicy","stage","decay","nerve","flier","shaft","crook","clean","china","ridge","vowel","gnome","snuck","icing","spiny","rigor","snail","flown","rabid","prose","thank","poppy","budge","fiber","moldy","dowdy","kneel","track","caddy","quell","dumpy","paler","swore","rebar","scuba","splat","flyer","horny","mason","doing","ozone","amply","molar","ovary","beset","queue","cliff","magic","truce","sport","fritz","edict","twirl","verse","llama","eaten","range","whisk","hovel","rehab","macaw","sigma","spout","verve","sushi","dying","fetid","brain","buddy","thump","scion","candy","chord","basin","march","crowd","arbor","gayly","musky","stain","dally","bless","bravo","stung","title","ruler","kiosk","blond","ennui","layer","fluid","tatty","score","cutie","zebra","barge","matey","bluer","aider","shook","river","privy","betel","frisk","bongo","begun","azure","weave","genie","sound","glove","braid","scope","wryly","rover","assay","ocean","bloom","irate","later","woken","silky","wreck","dwelt","slate","smack","solid","amaze","hazel","wrist","jolly","globe","flint","rouse","civil","vista","relax","cover","alive","beech","jetty","bliss","vocal","often","dolly","eight","joker","since","event","ensue","shunt","diver","poser","worst","sweep","alley","creed","anime","leafy","bosom","dunce","stare","pudgy","waive","choir","stood","spoke","outgo","delay","bilge","ideal","clasp","seize","hotly","laugh","sieve","block","meant","grape","noose","hardy","shied","drawl","daisy","putty","strut","burnt","tulip","crick","idyll","vixen","furor","geeky","cough","naive","shoal","stork","bathe","aunty","check","prime","brass","outer","furry","razor","elect","evict","imply","demur","quota","haven","cavil","swear","crump","dough","gavel","wagon","salon","nudge","harem","pitch","sworn","pupil","excel","stony","cabin","unzip","queen","trout","polyp","earth","storm","until","taper","enter","child","adopt","minor","fatty","husky","brave","filet","slime","glint","tread","steal","regal","guest","every","murky","share","spore","hoist","buxom","inner","otter","dimly","level","sumac","donut","stilt","arena","sheet","scrub","fancy","slimy","pearl","silly","porch","dingo","sepia","amble","shady","bread","friar","reign","dairy","quill","cross","brood","tuber","shear","posit","blank","villa","shank","piggy","freak","which","among","fecal","shell","would","algae","large","rabbi","agony","amuse","bushy","copse","swoon","knife","pouch","ascot","plane","crown","urban","snide","relay","abide","viola","rajah","straw","dilly","crash","amass","third","trick","tutor","woody","blurb","grief","disco","where","sassy","beach","sauna","comic","clued","creep","caste","graze","snuff","frock","gonad","drunk","prong","lurid","steel","halve","buyer","vinyl","utile","smell","adage","worry","tasty","local","trade","finch","ashen","modal","gaunt","clove","enact","adorn","roast","speck","sheik","missy","grunt","snoop","party","touch","mafia","emcee","array","south","vapid","jelly","skulk","angst","tubal","lower","crest","sweat","cyber","adore","tardy","swami","notch","groom","roach","hitch","young","align","ready","frond","strap","puree","realm","venue","swarm","offer","seven","dryer","diary","dryly","drank","acrid","heady","theta","junto","pixie","quoth","bonus","shalt","penne","amend","datum","build","piano","shelf","lodge","suing","rearm","coral","ramen","worth","psalm","infer","overt","mayor","ovoid","glide","usage","poise","randy","chuck","prank","fishy","tooth","ether","drove","idler","swath","stint","while","begat","apply","slang","tarot","radar","credo","aware","canon","shift","timer","bylaw","serum","three","steak","iliac","shirk","blunt","puppy","penal","joist","bunny","shape","beget","wheel","adept","stunt","stole","topaz","chore","fluke","afoot","bloat","bully","dense","caper","sneer","boxer","jumbo","lunge","space","avail","short","slurp","loyal","flirt","pizza","conch","tempo","droop","plate","bible","plunk","afoul","savoy","steep","agile","stake","dwell","knave","beard","arose","motif","smash","broil","glare","shove","baggy","mammy","swamp","along","rugby","wager","quack","squat","snaky","debit","mange","skate","ninth","joust","tramp","spurn","medal","micro","rebel","flank","learn","nadir","maple","comfy","remit","gruff","ester","least","mogul","fetch","cause","oaken","aglow","meaty","gaffe","shyly","racer","prowl","thief","stern","poesy","rocky","tweet","waist","spire","grope","havoc","patsy","truly","forty","deity","uncle","swish","giver","preen","bevel","lemur","draft","slope","annoy","lingo","bleak","ditty","curly","cedar","dirge","grown","horde","drool","shuck","crypt","cumin","stock","gravy","locus","wider","breed","quite","chafe","cache","blimp","deign","fiend","logic","cheap","elide","rigid","false","renal","pence","rowdy","shoot","blaze","envoy","posse","brief","never","abort","mouse","mucky","sulky","fiery","media","trunk","yeast","clear","skunk","scalp","bitty","cider","koala","duvet","segue","creme","super","grill","after","owner","ember","reach","nobly","empty","speed","gipsy","recur","smock","dread","merge","burst","kappa","amity","shaky","hover","carol","snort","synod","faint","haunt","flour","chair","detox","shrew","tense","plied","quark","burly","novel","waxen","stoic","jerky","blitz","beefy","lyric","hussy","towel","quilt","below","bingo","wispy","brash","scone","toast","easel","saucy","value","spice","honor","route","sharp","bawdy","radii","skull","phony","issue","lager","swell","urine","gassy","trial","flora","upper","latch","wight","brick","retry","holly","decal","grass","shack","dogma","mover","defer","sober","optic","crier","vying","nomad","flute","hippo","shark","drier","obese","bugle","tawny","chalk","feast","ruddy","pedal","scarf","cruel","bleat","tidal","slush","semen","windy","dusty","sally","igloo","nerdy","jewel","shone","whale","hymen","abuse","fugue","elbow","crumb","pansy","welsh","syrup","terse","suave","gamut","swung","drake","freed","afire","shirt","grout","oddly","tithe","plaid","dummy","broom","blind","torch","enemy","again","tying","pesky","alter","gazer","noble","ethos","bride","extol","decor","hobby","beast","idiom","utter","these","sixth","alarm","erase","elegy","spunk","piper","scaly","scold","hefty","chick","sooty","canal","whiny","slash","quake","joint","swept","prude","heavy","wield","femme","lasso","maize","shale","screw","spree","smoky","whiff","scent","glade","spent","prism","stoke","riper","orbit","cocoa","guilt","humus","shush","table","smirk","wrong","noisy","alert","shiny","elate","resin","whole","hunch","pixel","polar","hotel","sword","cleat","mango","rumba","puffy","filly","billy","leash","clout","dance","ovate","facet","chili","paint","liner","curio","salty","audio","snake","fable","cloak","navel","spurt","pesto","balmy","flash","unwed","early","churn","weedy","stump","lease","witty","wimpy","spoof","saner","blend","salsa","thick","warty","manic","blare","squib","spoon","probe","crepe","knack","force","debut","order","haste","teeth","agent","widen","icily","slice","ingot","clash","juror","blood","abode","throw","unity","pivot","slept","troop","spare","sewer","parse","morph","cacti","tacky","spool","demon","moody","annex","begin","fuzzy","patch","water","lumpy","admin","omega","limit","tabby","macho","aisle","skiff","basis","plank","verge","botch","crawl","lousy","slain","cubic","raise","wrack","guide","foist","cameo","under","actor","revue","fraud","harpy","scoop","climb","refer","olden","clerk","debar","tally","ethic","cairn","tulle","ghoul","hilly","crude","apart","scale","older","plain","sperm","briny","abbot","rerun","quest","crisp","bound","befit","drawn","suite","itchy","cheer","bagel","guess","broad","axiom","chard","caput","leant","harsh","curse","proud","swing","opine","taste","lupus","gumbo","miner","green","chasm","lipid","topic","armor","brush","crane","mural","abled","habit","bossy","maker","dusky","dizzy","lithe","brook","jazzy","fifty","sense","giant","surly","legal","fatal","flunk","began","prune","small","slant","scoff","torus","ninny","covey","viper","taken","moral","vogue","owing","token","entry","booth","voter","chide","elfin","ebony","neigh","minim","melon","kneed","decoy","voila","ankle","arrow","mushy","tribe","cease","eager","birth","graph","odder","terra","weird","tried","clack","color","rough","weigh","uncut","ladle","strip","craft","minus","dicey","titan","lucid","vicar","dress","ditch","gypsy","pasta","taffy","flame","swoop","aloof","sight","broke","teary","chart","sixty","wordy","sheer","leper","nosey","bulge","savor","clamp","funky","foamy","toxic","brand","plumb","dingy","butte","drill","tripe","bicep","tenor","krill","worse","drama","hyena","think","ratio","cobra","basil","scrum","bused","phone","court","camel","proof","heard","angel","petal","pouty","throb","maybe","fetal","sprig","spine","shout","cadet","macro","dodgy","satyr","rarer","binge","trend","nutty","leapt","amiss","split","myrrh","width","sonar","tower","baron","fever","waver","spark","belie","sloop","expel","smote","baler","above","north","wafer","scant","frill","awash","snack","scowl","frail","drift","limbo","fence","motel","ounce","wreak","revel","talon","prior","knelt","cello","flake","debug","anode","crime","salve","scout","imbue","pinky","stave","vague","chock","fight","video","stone","teach","cleft","frost","prawn","booty","twist","apnea","stiff","plaza","ledge","tweak","board","grant","medic","bacon","cable","brawl","slunk","raspy","forum","drone","women","mucus","boast","toddy","coven","tumor","truer","wrath","stall","steam","axial","purer","daily","trail","niche","mealy","juice","nylon","plump","merry","flail","papal","wheat","berry","cower","erect","brute","leggy","snipe","sinew","skier","penny","jumpy","rally","umbra","scary","modem","gross","avian","greed","satin","tonic","parka","sniff","livid","stark","trump","giddy","reuse","taboo","avoid","quote","devil","liken","gloss","gayer","beret","noise","gland","dealt","sling","rumor","opera","thigh","tonga","flare","wound","white","bulky","etude","horse","circa","paddy","inbox","fizzy","grain","exert","surge","gleam","belle","salvo","crush","fruit","sappy","taker","tract","ovine","spiky","frank","reedy","filth","spasm","heave","mambo","right","clank","trust","lumen","borne","spook","sauce","amber","lathe","carat","corer","dirty","slyly","affix","alloy","taint","sheep","kinky","wooly","mauve","flung","yacht","fried","quail","brunt","grimy","curvy","cagey","rinse","deuce","state","grasp","milky","bison","graft","sandy","baste","flask","hedge","girly","swash","boney","coupe","endow","abhor","welch","blade","tight","geese","miser","mirth","cloud","cabal","leech","close","tenth","pecan","droit","grail","clone","guise","ralph","tango","biddy","smith","mower","payee","serif","drape","fifth","spank","glaze","allot","truck","kayak","virus","testy","tepee","fully","zonal","metro","curry","grand","banjo","axion","bezel","occur","chain","nasal","gooey","filer","brace","allay","pubic","raven","plead","gnash","flaky","munch","dully","eking","thing","slink","hurry","theft","shorn","pygmy","ranch","wring","lemon","shore","mamma","froze","newer","style","moose","antic","drown","vegan","chess","guppy","union","lever","lorry","image","cabby","druid","exact","truth","dopey","spear","cried","chime","crony","stunk","timid","batch","gauge","rotor","crack","curve","latte","witch","bunch","repel","anvil","soapy","meter","broth","madly","dried","scene","known","magma","roost","woman","thong","punch","pasty","downy","knead","whirl","rapid","clang","anger","drive","goofy","email","music","stuff","bleep","rider","mecca","folio","setup","verso","quash","fauna","gummy","happy","newly","fussy","relic","guava","ratty","fudge","femur","chirp","forte","alibi","whine","petty","golly","plait","fleck","felon","gourd","brown","thrum","ficus","stash","decry","wiser","junta","visor","daunt","scree","impel","await","press","whose","turbo","stoop","speak","mangy","eying","inlet","crone","pulse","mossy","staid","hence","pinch","teddy","sully","snore","ripen","snowy","attic","going","leach","mouth","hound","clump","tonal","bigot","peril","piece","blame","haute","spied","undid","intro","basal","rodeo","guard","steer","loamy","scamp","scram","manly","hello","vaunt","organ","feral","knock","extra","condo","adapt","willy","polka","rayon","skirt","faith","torso","match","mercy","tepid","sleek","riser","twixt","peace","flush","catty","login","eject","roger","rival","untie","refit","aorta","adult","judge","rower","artsy","rural","shave","bobby","eclat","fella","gaily","harry","hasty","hydro","liege","octal","ombre","payer","sooth","unset","unlit","vomit","fanny","fetus","butch","stalk","flack","widow","augur","booby" };
std::vector<std::string> Perms = { "22222", "22221", "22220", "22212", "22211", "22210", "22202", "22201", "22200", "22122", "22121", "22120", "22112", "22111", "22110", "22102", "22101", "22100", "22022", "22021", "22020", "22012", "22011", "22010", "22002", "22001", "22000", "21222", "21221", "21220", "21212", "21211", "21210", "21202", "21201", "21200", "21122", "21121", "21120", "21112", "21111", "21110", "21102", "21101", "21100", "21022", "21021", "21020", "21012", "21011", "21010", "21002", "21001", "21000", "20222", "20221", "20220", "20212", "20211", "20210", "20202", "20201", "20200", "20122", "20121", "20120", "20112", "20111", "20110", "20102", "20101", "20100", "20022", "20021", "20020", "20012", "20011", "20010", "20002", "20001", "20000", "12222", "12221", "12220", "12212", "12211", "12210", "12202", "12201", "12200", "12122", "12121", "12120", "12112", "12111", "12110", "12102", "12101", "12100", "12022", "12021", "12020", "12012", "12011", "12010", "12002", "12001", "12000", "11222", "11221", "11220", "11212", "11211", "11210", "11202", "11201", "11200", "11122", "11121", "11120", "11112", "11111", "11110", "11102", "11101", "11100", "11022", "11021", "11020", "11012", "11011", "11010", "11002", "11001", "11000", "10222", "10221", "10220", "10212", "10211", "10210", "10202", "10201", "10200", "10122", "10121", "10120", "10112", "10111", "10110", "10102", "10101", "10100", "10022", "10021", "10020", "10012", "10011", "10010", "10002", "10001", "10000", "02222", "02221", "02220", "02212", "02211", "02210", "02202", "02201", "02200", "02122", "02121", "02120", "02112", "02111", "02110", "02102", "02101", "02100", "02022", "02021", "02020", "02012", "02011", "02010", "02002", "02001", "02000", "01222", "01221", "01220", "01212", "01211", "01210", "01202", "01201", "01200", "01122", "01121", "01120", "01112", "01111", "01110", "01102", "01101", "01100", "01022", "01021", "01020", "01012", "01011", "01010", "01002", "01001", "01000", "00222", "00221", "00220", "00212", "00211", "00210", "00202", "00201", "00200", "00122", "00121", "00120", "00112", "00111", "00110", "00102", "00101", "00100", "00022", "00021", "00020", "00012", "00011", "00010", "00002", "00001", "00000" };


int main() 
{

    std::vector<letterStruct> alphabet = returnAlphabet();
    std::string guess;
    std::string feedback;
    std::vector<std::string> valid_guesses = MasterList;
    std::vector<std::string> invalid_guesses;
    std::vector<std::pair<std::string, int>> sorted_guesses;


    while (true)
    {
        guess = getGuess();

        feedback = getFeedback();

        incorporateInfo(guess, feedback, &alphabet);

        removeIncorrectGuesses(&valid_guesses, getIncorrectGuesses(valid_guesses, alphabet));

        std::cout << "There are " << valid_guesses.size() << " words, to compare. Process will take *insert arbitrary number*\n";

        sorted_guesses = compareAllWords(valid_guesses);

        if (sorted_guesses.size() > 1)
        {
            std::cout << "List of words and their score: ";
            for (auto worddata : sorted_guesses)
            {
                std::cout << "\n\t[" << worddata.first << ", " << worddata.second << "]";
            }
        }
        else
        {
            std::cout << "The answer is: " << sorted_guesses[0].first << "\n";
            break;
        }
    }


}



std::vector<letterStruct> returnAlphabet() 
{
    std::vector<letterStruct> alphabet;
    for (int i = 0; i < 26; i++)
    {
        alphabet.push_back(letterStruct{ char(i + 97) });
    }
    return alphabet;
}

std::string getGuess()
{
    while (true)
    {
        std::cout << "\nWhat is your guess?\n";
        std::string guess;
        std::cin >> guess;
        if (std::find(MasterList.begin(), MasterList.end(), guess) != MasterList.end()) // apparently binary search is much faster for big bois, but some work (MIGHT BE worth it later, rn I'm just skeletoning) https://stackoverflow.com/a/571481
        {
            return guess;
        }
        else
        {
            std::cout << guess << " is not a valid guess. Try again.\n\n";
        }
        // check whether the guess is valid
    }
}

std::string getFeedback()
{
    std::vector<char> possible_inputs = { '0', '1', '2' };
    while (true)
    {
        std::cout << "Type your word's colors as 0 (grey), 1 (yellow), and 2 (green). Should look like this: \"00201\"\n";
        std::string feedback;
        std::cin >> feedback;
        //now check whether feedback works
        if (feedback.length() == 5 && feedback.find_first_not_of("012") == std::string::npos)
        {
            return feedback;
        }
        else
        {
            std::cout << "Feedback is not valid\n";
        }
    }
}


void incorporateInfo(const std::string guess, const std::string feedback, std::vector<letterStruct>* alphabet)
{ 
    for (int i = 0; i < 5; i++)
    {
        switch (feedback[i])
        {
            case '0':
                (*alphabet)[int(guess[i]) - 97].gray = true; // if the feedback says gray, say there can't be another one of these letters
                break;
            case '1':
                (*alphabet)[int(guess[i]) - 97].numLetters += 1;
                (*alphabet)[int(guess[i]) - 97].yellows.push_back(i);
                break;
            case '2':
                (*alphabet)[int(guess[i]) - 97].numLetters += 1;
                (*alphabet)[int(guess[i]) - 97].greens.push_back(i);
                break;
            default:
                std::cout << "something didn't work...\n" << "feedback " << i << " is " << feedback[i] << "; guess " << i << " is " << guess[i] << "\n";
        }
    }
}

void setAlphaInfo(std::map<char, std::vector<int>>* ref_yellows, std::map<char, std::vector<int>>* ref_greens, std::string* ref_grays, std::string* ref_doubles, const std::vector<letterStruct> alphabet)
{
    for (letterStruct letter : alphabet)
    {
        if (letter.gray)
        {
            if (letter.numLetters == 0)
            {
                (*ref_grays).push_back(letter.letter);
            }
            else
            {
                (*ref_doubles).push_back(letter.letter);
            }
        }
        if (letter.greens.size() > 0)
        {
            (*ref_greens)[letter.letter] = letter.greens;
        }
        if (letter.yellows.size() > 0)
        {
            (*ref_yellows)[letter.letter] = letter.yellows;
        }
    }
}

std::vector<std::string> getIncorrectGuesses(const std::vector<std::string> possible_guesses, const std::vector<letterStruct> alphabet)
{
    std::vector<std::string> wrong_guesses; // SHOULD I CYCLE THROUGH ALL OF THE POSSIBLE WORDS once AND ELIMINATE THEM? YES!!!

    std::map<char, std::vector<int>> yellows;
    std::map<char, std::vector<int>> greens;
    std::string grays;
    std::string doubles;


    setAlphaInfo(&yellows, &greens, &grays, &doubles, alphabet);



    for (std::string word : possible_guesses)  
    {

        if (std::find(wrong_guesses.begin(), wrong_guesses.end(), word) == wrong_guesses.end()) // if word is not in wrong_guesses, process it (safety net)
        {
            for (char gray_letter : grays)
            {
                if (std::count(word.begin(), word.end(), gray_letter)) // if the word has a gray letter, eliminate it and continue;
                {
                    //std::cout << word << " was eliminated because it has " << gray_letter << " (a gray letter)\n";
                    wrong_guesses.push_back(word);
                    goto nxtword;
                }
            }
            for (const auto& kv : yellows) // for all the yellow letters
            {
                if (!std::count(word.begin(), word.end(), kv.first)) // if word does not have the yellow letter, eliminate it and continue
                {
                    //std::cout << word << " was eliminated because it does not have " << kv.first << " (a yellow letter)\n";
                    wrong_guesses.push_back(word);
                    goto nxtword;
                }
                else // if word DOES have yellow letter, check if it is in the wrong place
                {
                    for (int place : kv.second) // for every int in every yellow's locations
                    {
                        if (std::distance(word.begin(), std::find(word.begin(), word.end(), kv.first)) == place) // if the (first) location of the letter in word is the same as a yellow's place, eliminate it and continue
                        {
                            //std::cout << word << " was eliminated because it has " << kv.first << " (a yellow letter) in " << place << "(the wrong place)\n";
                            wrong_guesses.push_back(word);
                            goto nxtword;
                        }
                    }
                }
            }
            for (const auto& kv : greens) // for all the green letters
            {
                if (!std::count(word.begin(), word.end(), kv.first)) // if word does not have the green letter, eliminate it and continue
                {
                    //std::cout << word << " was eliminated because it does not have " << kv.first << " (a green letter)\n";
                    wrong_guesses.push_back(word);
                    goto nxtword;
                }
                else // if word DOES have green letter, check if it is in the wrong place
                {
                    for (int place : kv.second) // for every int in every green's locations
                    {
                        if (std::distance(word.begin(), std::find(word.begin(), word.end(), kv.first)) != place) // if the (first) location of the letter in word is NOT the same as a green's place, eliminate it and continue
                        {
                            //std::cout << word << " was eliminated because it has " << kv.first << " (a green letter) in " << std::distance(word.begin(), std::find(word.begin(), word.end(), kv.first)) << "(the wrong place)\n";
                            wrong_guesses.push_back(word);
                            goto nxtword;
                        }
                    }
                }
            }


                    // MAKE DOUBLES WORK SOMEHOW PLEASE.......... PROBABLY HAVE TO PUT DOUBLES FIRST
        }

    nxtword:
        continue;
    }
    return wrong_guesses;

}

void removeIncorrectGuesses(std::vector<std::string>* possible_guesses, const std::vector<std::string> wrong_guesses)
{
    for (std::string wrong : wrong_guesses)
    {
        auto found = std::find(possible_guesses->begin(), possible_guesses->end(), wrong);
        if (found != possible_guesses->end())
        {
            possible_guesses->erase(std::distance(possible_guesses->begin(), found) + possible_guesses->begin());
            //std::cout << wrong << " is supposed to have been removed\n";
        }
        else
        {
            std::cout << "something stinky with " << wrong << "\n";
        }
    }
}

std::vector<int> getGuessLogistics(const std::string guess, const std::vector<std::string> guesses_possible)
{
    std::vector<int> wrong_guess_len;
    std::vector<letterStruct> change_alpha;
    for (std::string perm : Perms)
    {
         change_alpha = returnAlphabet();
         incorporateInfo(guess, perm, &change_alpha);
         wrong_guess_len.push_back(getIncorrectGuesses(guesses_possible, change_alpha).size());
    }
    return wrong_guess_len;
}

void sortGuessLogistics(std::vector<int>* guess_log)
{
    std::sort(guess_log->begin(), guess_log->end());
    std::reverse(guess_log->begin(), guess_log->end());
}

void pruneGuessLogistics(std::vector<int>* sorted_guess_log, const int max_wrongs)
{
    sorted_guess_log->erase(std::remove(sorted_guess_log->begin(), sorted_guess_log->end(), max_wrongs), sorted_guess_log->end());
}

int getGuessLogisticStatistics(const std::vector<int> pruned_guess_log)
{
    return std::accumulate(pruned_guess_log.begin(), pruned_guess_log.end(), 0) / pruned_guess_log.size();
}

bool compareSecondValues(std::pair<std::string, int>& a, std::pair<std::string, int>& b)
{
    return a.second < b.second;
}

std::vector<std::pair<std::string, int>> compareAllWords(const std::vector<std::string> possible_words)
{
    std::vector<std::pair<std::string, int>> word_map;
    std::vector<int> guess_log;
    std::cout << "Processing Words...\n";
    for (int i = 0; i < possible_words.size(); i++) // int i = 0; i < possible_words.size(); i++
    {
        guess_log = getGuessLogistics(possible_words[i], possible_words);
        sortGuessLogistics(&guess_log);
        pruneGuessLogistics(&guess_log, possible_words.size());
        word_map.push_back(std::make_pair(possible_words[i], getGuessLogisticStatistics(guess_log))); // add a progress bar
        if (i % int(ceil(double(possible_words.size())/10)) == 0)
        {
            std::cout << round((i / double(possible_words.size())) * 10)*10 << "% complete\n";
        }
    }
    sort(word_map.begin(), word_map.end(), compareSecondValues);
    std::reverse(word_map.begin(), word_map.end());
    return word_map; 
}


// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
