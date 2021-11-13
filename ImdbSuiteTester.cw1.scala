package imdb

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class ImdbSuiteTester extends AnyFunSuite with BeforeAndAfterAll {
  def initializeImdb(): Boolean =
    try {
      ImdbAnalysis
      true
    } catch {
      case ex: Throwable =>
        println(ex.getMessage)
        ex.printStackTrace()
        false
    }

  val INIT_ERR_MSG = " -- did you fill in all the values in ImdbAnalysis (titleBasicsList, titleRatingsList, titleCrewList, nameBasicsList)?"

  override def afterAll(): Unit = {
    assert(initializeImdb(), INIT_ERR_MSG)
  }

  /**
   * Creates a truncated string representation of a list, adding ", ...)" if there
   * are too many elements to show
   * @param l The list to preview
   * @param n The number of elements to cut it at
   * @return A preview of the list, containing at most n elements.
   */
  def previewList[A](l: List[A], n: Int = 10): String =
    if (l.length <= n) l.toString
    else l.take(n).toString.dropRight(1) + ", ...)"

  /**
   * Asserts that all the elements in a given list and an expected list are the same,
   * regardless of order. For a prettier output, given and expected should be sorted
   * with the same ordering.
   * @param given The actual list
   * @param expected The expected list
   * @tparam A Type of the list elements
   */
  def assertSameElements[A](given: List[A], expected: List[A]): Unit = {
    val givenSet = given.toSet
    val expectedSet = expected.toSet

    val unexpected = givenSet -- expectedSet
    val missing = expectedSet -- givenSet

    val noUnexpectedElements = unexpected.isEmpty
    val noMissingElements = missing.isEmpty

    val noMatchString =
      s"""
         |Expected: ${previewList(expected)}
         |Actual:   ${previewList(given)}""".stripMargin

    assert(noUnexpectedElements,
      s"""|$noMatchString
          |The given collection contains some unexpected elements: ${previewList(unexpected.toList, 5)}""".stripMargin)

    assert(noMissingElements,
      s"""|$noMatchString
          |The given collection is missing some expected elements: ${previewList(missing.toList, 5)}""".stripMargin)
  }

    def assertSameElementsTask1(given: List[(Float, Int, Int, String)], expected: List[(Float, Int, Int, String)]): Unit = {
      def round1(x: (Float, Int, Int, String)): (Int, Int, Int, String) = ((x._1 * 6).toInt, x._2, x._3, x._4)
      assertSameElements(given.map(round1), expected.map(round1))
    }

    test("Task1") {
      assert(initializeImdb(), INIT_ERR_MSG)
      val list = ImdbAnalysis.titleBasicsList
      val expectedResult =
        List((103.335144,1,3122,"History"), (66.02816,0,3900,"Comedy"), (87.51441,1,2925,"Action"), (58.799427,1,1440,"News"), (92.91029,1,1620,"War"), (78.26385,1,1080,"Horror"),
          (22.011599,0,1248,"Animation"), (94.11684,1,840,"Biography"), (63.46959,1,384,"Western"), (93.90296,1,2002,"Adult"), (84.3144,1,1442,"Crime"), (72.05768,1,877,"Sci-Fi"),
          (52.664017,3,720,"Reality-TV"), (83.739334,1,450,"Musical"), (79.99037,1,2925,"Adventure"), (75.762085,1,5220,"Music"), (83.50197,51,135,"Film-Noir"), (15.649951,0,110,"Short"),
          (85.49311,0,3900,"Drama"), (89.14799,1,1800,"Romance"), (52.160397,0,3900,"Family"), (91.55019,1,780,"Thriller"), (51.512554,0,8400,"Documentary"), (73.03895,1,999,"Fantasy"),
          (59.774086,7,1020,"Talk-Show"), (83.08545,1,877,"Mystery"), (44.027664,1,300,"Game-Show"), (76.81902,1,1140,"Sport"))
      val floatedExpected = expectedResult.map(x => (x._1.toFloat, x._2, x._3, x._4))
      assertSameElementsTask1(ImdbAnalysis.task1(list), floatedExpected)
    }

    test("Task2") {
      assert(initializeImdb(), INIT_ERR_MSG)
      val list1 = ImdbAnalysis.titleBasicsList
      val list2 = ImdbAnalysis.titleRatingsList
      val res = ImdbAnalysis.task2(list1, list2)
      assertSameElements(res,
        List("Goodfellas", "Home Alone", "The Silence of the Lambs", "Terminator 2: Judgment Day", "Reservoir Dogs", "Groundhog Day", "Jurassic Park", "Schindler's List", "Forrest Gump",
          "The Lion King", "Léon: The Professional", "Pulp Fiction", "The Shawshank Redemption", "Braveheart", "Heat", "Se7en", "Toy Story", "12 Monkeys", "The Usual Suspects", "Fargo",
          "Trainspotting", "The Big Lebowski", "Life Is Beautiful", "Good Will Hunting", "L.A. Confidential", "Titanic", "Toy Story 2", "The Truman Show", "American History X", "The Green Mile",
          "Lock, Stock and Two Smoking Barrels", "The Lord of the Rings: The Fellowship of the Ring", "Saving Private Ryan", "Star Wars: Episode III - Revenge of the Sith", "Shrek", "The Matrix",
          "Fight Club", "American Psycho", "Cast Away", "The Lord of the Rings: The Return of the King", "The Lord of the Rings: The Two Towers", "The Sixth Sense", "American Beauty", "Gladiator",
          "Requiem for a Dream", "Minority Report", "Monsters, Inc.", "Snatch", "Memento", "Amélie", "Ocean's Eleven", "Harry Potter and the Sorcerer's Stone", "Spirited Away", "Donnie Darko",
          "The Pianist", "The Bourne Identity", "Catch Me If You Can", "Finding Nemo", "Kill Bill: Vol. 1", "A Beautiful Mind", "Harry Potter and the Prisoner of Azkaban", "City of God",
          "The Incredibles", "Pirates of the Caribbean: The Curse of the Black Pearl", "Harry Potter and the Goblet of Fire", "The Notebook", "Eternal Sunshine of the Spotless Mind",
          "Inglourious Basterds", "Oldboy", "Shaun of the Dead", "Iron Man", "Batman Begins", "Harry Potter and the Order of the Phoenix", "Kill Bill: Vol. 2", "Casino Royale", "Ratatouille",
          "Sin City", "Million Dollar Baby", "The Departed", "Watchmen", "300", "The Curious Case of Benjamin Button", "V for Vendetta", "Toy Story 3", "The Bourne Ultimatum", "Blood Diamond",
          "Life of Pi", "Pan's Labyrinth", "The Dark Knight", "There Will Be Blood", "Ex Machina", "No Country for Old Men", "The Prestige", "Avatar")
      )
    }

    test("Task3") {
      assert(initializeImdb(), INIT_ERR_MSG)
      val list1 = ImdbAnalysis.titleBasicsList
      val list2 = ImdbAnalysis.titleRatingsList
      val res = ImdbAnalysis.task3(list1, list2)
      val expectedResult = List(
        (0,"Action","The Story of the Kelly Gang"), (0,"Adventure","The Story of the Kelly Gang"), (0,"Biography","The Story of the Kelly Gang"),
        (0,"Comedy","Um Cavalheiro Deveras Obsequioso"), (0,"Documentary","België"), (0,"Drama","Heroes of the Cross"), (0,"Family","The Life of Moses"),
        (0,"Fantasy","The Fairylogue and Radio-Plays"), (0,"History","The Scottish Covenanters"), (0,"Music","Faust"), (0,"Musical","Highlights from The Mikado"),
        (0,"News","May Day Parade"), (0,"Romance","A Viúva Alegre"), (0,"Sport","Jeffries and Ruhlin Sparring Contest at San Francisco, Cal., November 15, 1901"),
        (0,"War","Gøngehøvdingen"), (1,"Action","The Scarlet Runner"), (1,"Adult","The Goat"), (1,"Adventure","The Dark Star"), (1,"Animation","El apóstol"),
        (1,"Biography","The Fall of the Romanoffs"), (1,"Comedy","Seventeen"), (1,"Crime","It Happened in Paris"), (1,"Documentary","Ayastefanos'taki Rus Abidesinin Yikilisi"),
        (1,"Drama","Arms and the Woman"), (1,"Family","The Blue Bird"), (1,"Fantasy","Peer Gynt"), (1,"History","War and Peace"), (1,"Horror","J'accuse!"),
        (1,"Musical","Excelsior"), (1,"Mystery","The Devil-Stone"), (1,"Romance","On the Quiet"), (1,"Sci-Fi","A Trip to Mars"), (1,"Sport","1915 World's Championship Series"),
        (1,"Thriller","The Mutiny of the Bounty"), (1,"War","On Dangerous Ground"), (1,"Western","Revenge"), (2,"Action","Frozen River"), (2,"Adult","Le ménage moderne de Madame Butterfly"),
        (2,"Adventure","Ginsberg the Great"), (2,"Animation","The Adventures of Prince Achmed"), (2,"Biography","Napoleon"), (2,"Comedy","Sonny Boy"), (2,"Crime","Queen of the Night Clubs"),
        (2,"Documentary","Hunger... Hunger... Hunger"), (2,"Drama","Queen of the Night Clubs"), (2,"Family","The Kid"), (2,"Fantasy","Die Nibelungen: Siegfried"), (2,"Film-Noir","Underworld"),
        (2,"History","Napoleon"), (2,"Horror","Faust"), (2,"Music","Man with a Movie Camera"), (2,"Musical","Queen of the Night Clubs"), (2,"Mystery","The Cabinet of Dr. Caligari"),
        (2,"News","Felix Hits the North Pole"), (2,"Romance","Hold Your Man"), (2,"Sci-Fi","Metropolis"), (2,"Short","In the Good Old Summer Time"), (2,"Sport","Johnny Get Your Hair Cut"),
        (2,"Thriller","Greed"), (2,"War","The Big Parade"), (2,"Western","A One Man Game"), (3,"Action","Law of the Texan"), (3,"Adventure","Chucho el Roto"), (3,"Animation","The Story of the Fox"),
        (3,"Biography","Thiruneelakantar"), (3,"Comedy","L'Ordonnance malgré lui"), (3,"Crime","Der vierte kommt nicht"), (3,"Documentary","Festas de Homenagem do Futebol Clube do Porto à Rainha da Colónia Portuguesa no Brasil"),
        (3,"Drama","Bhakta Jayadeva"), (3,"Family","Modern Times"), (3,"Fantasy","The Wizard of Oz"), (3,"Film-Noir","I Am a Fugitive from a Chain Gang"), (3,"History","La chanson de l'adieu"), (3,"Horror","Freaks"),
        (3,"Music","Romance in the Dark"), (3,"Musical","Bhakta Jayadeva"), (3,"Mystery","M"), (3,"News","Baboona"), (3,"Romance","Fugitive Lady"), (3,"Sci-Fi","King Kong"), (3,"Short","Hockey: Canada's National Game"),
        (3,"Sport","Fighting Thoroughbreds"), (3,"Thriller","M"), (3,"War","End of the Trail"), (3,"Western","Trailing North"), (4,"Action","Kamagong: Bayani ng Mahirap"), (4,"Adventure","The Treasure of the Sierra Madre"),
        (4,"Animation","Fantasia"), (4,"Biography","Vladimir Ilich Lenin"), (4,"Comedy","Melodies of America"), (4,"Crime","Double Indemnity"), (4,"Documentary","One Inch from Victory"), (4,"Drama","Nandanar"),
        (4,"Family","It's a Wonderful Life"), (4,"Fantasy","It's a Wonderful Life"), (4,"Film-Noir","Double Indemnity"), (4,"History","Nazi Concentration and Prison Camps"), (4,"Horror","Dead of Night"),
        (4,"Music","St. Matthew Passion"), (4,"Musical","Melodies of America"), (4,"Mystery","Mohan"), (4,"Romance","Casablanca"), (4,"Sci-Fi","Krakatit"), (4,"Short","The Nazis Strike"),
        (4,"Sport","Heroes of the Saddle"), (4,"Thriller","Rome, Open City"), (4,"War","One Inch from Victory"), (4,"Western","Stallion Canyon"), (5,"Action","Nufarul rosu"), (5,"Adventure","Seven Samurai"),
        (5,"Animation","Amazon Symphony"), (5,"Biography","Mahatma Phule"), (5,"Comedy","Mayabazar"), (5,"Crime","12 Angry Men"), (5,"Documentary","Pinne e arpioni"), (5,"Drama","Pavankhind"), (5,"Family","Bhaktha Vijaya"),
        (5,"Fantasy","Umer Marvi"), (5,"Film-Noir","Sunset Blvd."), (5,"History","The Book of Acts Series"), (5,"Horror","Diabolique"), (5,"Music","Díszelöadás"), (5,"Musical","Buenos Aires a la vista"),
        (5,"Mystery","Rear Window"), (5,"Romance","Meine 99 Bräute"), (5,"Sci-Fi","Sharey Chuattar"), (5,"Sport","Los peloteros"), (5,"Thriller","Rear Window"), (5,"War","Victory at Sea"), (5,"Western","The Old Frontier"),
        (6,"Action","Lastik Man"), (6,"Adult","A Man, Eight Girls"), (6,"Adventure","The Adventures of Goopy and Bagha"), (6,"Animation","Havoc in Heaven"), (6,"Biography","Niko Pirosmanishvili"),
        (6,"Comedy","O Palhaço O Que É?"), (6,"Crime","Hei mei gui"), (6,"Documentary","Partizanska igra"), (6,"Drama","Mother Dearest"), (6,"Family","The Adventures of Goopy and Bagha"), (6,"Fantasy","Lastik Man"),
        (6,"History","The Human Condition III: A Soldier's Prayer"), (6,"Horror","Psycho"), (6,"Music","Der Freischütz"), (6,"Musical","The Tony Fontane Story"), (6,"Mystery","Hara-Kiri"),
        (6,"News","Crisis: Behind a Presidential Commitment"), (6,"Romance","World Gone Mad"), (6,"Sci-Fi","2001: A Space Odyssey"), (6,"Short","Why Vietnam?"), (6,"Sport","Rocco and His Brothers"),
        (6,"Thriller","Le Trou"), (6,"War","W.I.A. Wounded in Action"), (6,"Western","The Good, the Bad and the Ugly"), (7,"Action","O Judoka"), (7,"Adult","Don't Fight It, Kid"), (7,"Adventure","O Judoka"),
        (7,"Animation","The Pinchcliffe Grand Prix"), (7,"Biography","Raja Harishchandrra"), (7,"Comedy","The Chaos Class"), (7,"Crime","The Godfather"), (7,"Documentary","Always a New Beginning"),
        (7,"Drama","The Chaos Class"), (7,"Family","Isa-dalawa-tatlo: Ang tatay kong kalbo"), (7,"Fantasy","Daana Veera Soora Karna"), (7,"History","Raja Harishchandrra"), (7,"Horror","Alien"),
        (7,"Music","Pink Floyd: Live at Pompeii"), (7,"Musical","Nagara Haavu"), (7,"Mystery","Süt Kardesler"), (7,"News","Cup Glory"), (7,"Reality-TV","As Armas e o Povo"), (7,"Romance","Bakit may pag-ibig pa?"),
        (7,"Sci-Fi","Alien"), (7,"Sport","The Speed Merchants"), (7,"Talk-Show","Revenge of the Motorcycle Mama"), (7,"Thriller","Chinatown"), (7,"War","The Fifth Seal"), (7,"Western","The Marshal of Windy Hollow"),
        (8,"Action","Ang pumatay ng dahil sa iyo"), (8,"Adult","Dresden Diary 3"), (8,"Adventure","Who's Singin' Over There?"), (8,"Animation","Grave of the Fireflies"), (8,"Biography","Who Shot President Kennedy?"),
        (8,"Comedy","Rock-N-America"), (8,"Crime","The Untouchable Family"), (8,"Documentary","Disappearing Oasis, Last Oasis"), (8,"Drama","Në çdo stinë"), (8,"Family","Fillim i vështirë"),
        (8,"Fantasy","Star Wars: Episode V - The Empire Strikes Back"), (8,"History","Who Shot President Kennedy?"), (8,"Horror","The Shining"), (8,"Music","Juju Music"), (8,"Musical","Chithram"),
        (8,"Mystery","Xue jian hua ping"), (8,"Romance","Në çdo stinë"), (8,"Sci-Fi","Abandonada"), (8,"Sport","Raging Bull"), (8,"Thriller","Opasen char"), (8,"War","Entertaining the Troops"),
        (8,"Western","Sadiyata"), (9,"Action","Por un salvaje amor"), (9,"Adult","Girls Loving Girls"), (9,"Adventure","Ramayana: The Legend of Prince Rama"), (9,"Animation","Ramayana: The Legend of Prince Rama"),
        (9,"Biography","Diego Rivera: I Paint What I See"), (9,"Comedy","Mohammed's Radio"), (9,"Crime","Pulp Fiction"), (9,"Documentary","Aco, odluci se: Slavko Sohaj iznedu dvije ljubavi"), (9,"Drama","The Shawshank Redemption"),
        (9,"Family","Mi Puerto Rico"), (9,"Fantasy","The Green Mile"), (9,"History","Schindler's List"), (9,"Horror","Manichithrathazhu"), (9,"Music","Mi Puerto Rico"), (9,"Musical","Edu Kondalaswamy"),
        (9,"Mystery","In Harihar Nagar"), (9,"Romance","Isang tanong, isang sagot"), (9,"Sci-Fi","The Matrix"), (9,"Short","A Worn Path"), (9,"Sport","Freestyle: The Victories of Dan Gable"), (9,"Thriller","Devasuram"), (9,"War","Aguner Poroshmoni"), (9,"Western","Unforgiven")
      )
      assert(res == expectedResult)
    }

    test("Task4") {
      assert(initializeImdb(), INIT_ERR_MSG)
      val list1 = ImdbAnalysis.titleBasicsList
      val list2 = ImdbAnalysis.titleCrewList
      val list3 = ImdbAnalysis.nameBasicsList
      val res = ImdbAnalysis.task4(list1, list2, list3)
      val expectedResult = List(
        ("Karimah Westbrook",2), ("Moustapha Alassane",2), ("Alexa Alden",2), ("Ted Alvarez",2), ("Helen Barrett",2), ("Daryl Bartley",2), ("Rosie Bedford-Stradling",2),
        ("Jamie Bishop",2), ("Philip Borg",2), ("Jess Brackenburry",2), ("Kevin Brink",2), ("Chris Burdon",2), ("C. Douglas Cameron",2), ("Robert C. Campion",2), ("Richard L. Carden",2),
        ("Robert J. Carlyle",2), ("David Checel",2), ("Nigel Churcher",2), ("Jeffrey N. Civa",2), ("Michael Coldewey",2), ("Sean Connor",2), ("Steve Cook",2), ("Rosy Coppola",2),
        ("Nick Costantino",2), ("Steve Cullane",2), ("Louis D'Esposito",2), ("Danielle da Costa",2), ("Liz Dann",2), ("Darrell Davis",2), ("Jay Duerr",2), ("Peter Edge",2), ("Colin Ellis",2),
        ("Kiko Ellsworth",2), ("Rory Enke",2), ("Mark 'Rocky' Evans",2), ("Warren Evans",2), ("Oliver Exmundo",2), ("Sean Fairburn",2), ("Jesse Farrelly",2), ("Andrew P. Flores",2),
        ("David Ford",2), ("Todd Forsberg",2), ("Paul Fox",2), ("Grace Fong",2), ("Brendan Garst",2), ("Dawn Michelle King",2), ("Gary Gleeson",2), ("Sean Goldman",2), ("Rhona Gordon",2),
        ("Derek Grabski",2), ("Joseph Graham",2), ("Sarah Green",2), ("Judy Greer",2), ("Nicky Gregory",2), ("Dax Griffin",2), ("Charles Guanci Jr.",2), ("Duane Gullison",2),
        ("Mark Hadland",2), ("Louisa Gore Hamn",2), ("Edward T. Hanley",2), ("Greg Harris",2), ("Phil Harvey",2), ("Harlon Haveland",2), ("Colin Hazell",2), ("Stephen McKinley Henderson",2),
        ("Jason Herschaft",2), ("John Hicks",2), ("Elizabeth Hipwell",2), ("Kelly Hirano",2), ("Nick Hobbs",2), ("Jason Horwood",2), ("Sammy Sheldon",2), ("Gary Hymns",2), ("Joe Inscoe",2),
        ("Margarita Jeannot",2), ("Chris Joehnk",2), ("Scott F. Johnston",2), ("John Jordan",2), ("John Kairis",2), ("Shari Ratliff",2), ("Alex King",2), ("Kelly Klindt",2), ("Hilary Klym",2),
        ("Adam Lagattuta",2), ("Martin Lane",2), ("Andrew Langton",2), ("Eric Leach",2), ("Dan Lebental",2), ("John Lockwood",2), ("Larry Long",2), ("Kevin Loo",2), ("Ernest Lopez",2),
        ("Alvin William 'Dutch' Lunak",2), ("Francesco Lupica",2))
      assertSameElements(res, expectedResult)
    }

    def timeIt[T](label: String, code: => T): Long = {
      val start = System.currentTimeMillis()
      val result = code
      val stop = System.currentTimeMillis()
      stop - start
    }

    def padTo4Left(content: String): String = {
      " " * (4 - content.length) ++ content
    }

    def formatValue(value: Long): String = {
      val str = value.toString
      val col = if (value <= 75) scala.io.AnsiColor.GREEN else if (value >= 150) scala.io.AnsiColor.RED
      else scala.io.AnsiColor.WHITE
      " " * (4 - str.length) ++ col ++ str ++ scala.io.AnsiColor.RESET
    }

    def makeRow(content: List[String]): String = {
      content.foldLeft("│"){ (acc, obj) =>
        acc ++ " " ++ obj ++ " │"
      }
    }

    def makeTable(content: List[List[String]]): String = {
      val topRow = "┌────────┬──────┬──────┬──────┐\n"
      val midRow = "├────────┼──────┼──────┼──────┤\n"
      val botRow = "└────────┴──────┴──────┴──────┘\n"
      val table = content.foldLeft(topRow){ (acc, obj) =>
        acc ++ makeRow(obj.map(padTo4Left)) ++ "\n" ++ midRow
      }
      table.dropRight(midRow.length) ++ botRow
    }

    def makeProgress(current: Double, total: Double, length: Int) = {
      val percent = current * 100 / total
      val arrow   = "█" * (percent/100 * length ).toInt// + ' '
      val spaces  = "░" * (length - arrow.length)

      print("Benchmarking: " + arrow + spaces + " +" + percent.toInt.toString + "%.\r")
    }

    test("Running overall benchmark") {
      assert(initializeImdb(), INIT_ERR_MSG)

      import ImdbAnalysis._

      var results: (List[Long], List[Long], List[Long], List[Long]) = (List(), List(), List(), List())
      val runs = 20
      var i = 0
      while(i < runs) {
        makeProgress(i, runs - 1, 40)
        val t1 = timeIt("Task 1", task1(titleBasicsList))
        val t2 = timeIt("Task 2", task2(titleBasicsList, titleRatingsList))
        val t3 = timeIt("Task 3", task3(titleBasicsList, titleRatingsList))
        val t4 = timeIt("Task 4", task4(titleBasicsList, titleCrewList, nameBasicsList))
        results = (t1 :: results._1, t2 :: results._2, t3 :: results._3, t4 :: results._4)
        i += 1
      }
      println() // Print line to fix \r
      val labels = List("Task  ", "Avg", "Min", "Max")
      val values = List(results._1, results._2, results._3, results._4)
      val tab = values.zipWithIndex.map{ case (values, index) =>
        List[String](
          "Task " ++ (index+1).toString,
          formatValue(values.sum / values.length),
          formatValue(values.min),
          formatValue(values.max)) }
      println(makeTable(labels :: tab))
      assert(true)
    }
}
