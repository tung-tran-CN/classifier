object experiment {

	def connect(env: String)(server: String): String = {
		println("Env=" + env + ", server=" + server)
		"author"
	}                                         //> connect: (env: String)(server: String)String
	
	def x = connect("A")(_)                   //> x: => String => String
	x ("C")                                   //> Env=A, server=C
                                                  //| res0: String = author


	def as(credential: String) = {
		println("Cred=" + credential)
		1
	}                                         //> as: (credential: String)Int
	as("admin")                               //> Cred=admin
                                                  //| res1: Int = 1

	"/etc/tags/vanityfair" substring "/etc/tags/vanityfair/".length
                                                  //> java.lang.StringIndexOutOfBoundsException: String index out of range: -1
                                                  //| 	at java.lang.String.substring(String.java:1937)
                                                  //| 	at java.lang.String.substring(String.java:1904)
                                                  //| 	at experiment$$anonfun$main$1.apply$mcV$sp(experiment.scala:18)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at experiment$.main(experiment.scala:1)
                                                  //| 	at experiment.main(experiment.scala)
	
	List(1,2,3) map (n => n * 2)
}