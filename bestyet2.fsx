//opg 11g
//Christopher Kyed
//Daniel Van
//Steffen Van

open System.Windows.Forms
open System.Drawing
open System
type pen = Color * float
let mutable temp = []
let mutable planetColors = []
let mutable names = []

type Planet (filename:string, color : Brush) = class
  //Variabler der hjælper med at kunne hive et element ud af en triple.
  let first (a, _, _) = a
  let second (_, b, _) = b
  let third (_, _, c) = c

  //Konstaner til udregninger.
  
  //let AU = 149597870700 //m
  //let GravityK = 6.67384 * (10.0**(-11.0)) //m³/(s²*kg)
  let GMsun = 2.959122082322128 * (10.0**(-4.0)) //AU³/day²
  let convert = Math.PI/180.0

  //Åbner en stream til tekstfilen, læser hele filen og laver et array af de strings vi er interessede i.
  let read = System.IO.File.OpenText filename
  let removeEmpty = StringSplitOptions.RemoveEmptyEntries
  let fulltext = read.ReadToEnd()
  let startIndex = fulltext.IndexOf("$$SOE") + 5
  let endIndex = fulltext.LastIndexOf("$$EOE") - 5
  let usabletext = fulltext.[startIndex..endIndex]
  let arraydata = usabletext.Split ([|' '; '\r'|], removeEmpty) 
  
  //Positionerne for den første dag i 2016
  let long = float arraydata.[91311]
  let lat = float arraydata.[91312] + 90.0
  let rad = float arraydata.[91313]
  //Positionerne for den anden dag i 2016
  let sndlong = float arraydata.[91316]
  let sndlat = float arraydata.[91317] + 90.0
  let sndrad = float arraydata.[91318]
  
  //Brugt til at kunne ændre long, lat og rad så værdier for forskellige dage. 
  //kan printes nede I NasaData funktionen.
  let mutable longData = 0.0
  let mutable latData = 0.0
  let mutable radData = 0.0

  //Omregninger fra sfæriske koordinater til kartetiske koordinater.
  member this.Xcord = rad * (sin(lat*convert)) * (cos(long*convert))
  member this.Ycord = rad * (sin(lat*convert)) * (sin(long*convert))
  member this.Zcord = rad * (cos(lat*convert))
  
  member this.Xcord2 = sndrad * (sin(sndlat*convert)) * (cos(sndlong*convert))
  member this.Ycord2 = sndrad * (sin(sndlat*convert)) * (sin(sndlong*convert))
  member this.Zcord2 = sndrad * (cos(sndlat*convert)) 

  member this.XcordData = radData * (sin(latData*convert)) * (cos(longData*convert))
  member this.YcordData = radData * (sin(latData*convert)) * (sin(longData*convert))
  member this.ZcordData = radData * (cos(latData*convert))

  member this.Position = (this.Xcord, this.Ycord, this.Zcord)
  member this.Position2 = (this.Xcord2, this.Ycord2, this.Zcord2)
  member this.DataPosition = (this.XcordData, this.YcordData, this.ZcordData)

  //Formlen for Acceleration fra opgaven.
  member this.AccEq(position) = 
    -(GMsun/((sqrt ((first position**2.0)+(second position**2.0)+(third position**2.0)))**3.0))

  //Ganger regnestykket med positionsvektoren.
  member this.Accelerate =
    (this.AccEq(this.Position) * first this.Position, this.AccEq(this.Position) * second this.Position, this.AccEq(this.Position) * third this.Position)

  //Trækker 2 positionsvektorer fra hinanden for at bestemme distancen.
  member this.Distance = 
    let fstPos = first this.Position
    let sndPos = second this.Position
    let trdPos = third this.Position
    (abs (fstPos - first this.Position2), abs (sndPos - second this.Position2), abs (trdPos - third this.Position2))

  //Tager distancen og dividerer den med forskellen i tidsskridt. 
  member this.Velocity =
    (first this.Distance/(1.0 - 0.0), second this.Distance/(1.0 - 0.0), third this.Distance/(1.0 - 0.0))
    
  //Simulations funtionen som kan simulere en planets fremtidige positioner, 
  //ud fra 2 konkrete positionsekesmpler. 
  //Den kan simulere i op til et år og i intervaller fra hver dag tilhver 30. dag.
  member this.Simulate(simtime:int) (interval:int) =
    let mutable planetList = []
    let mutable vel = this.Velocity
    let mutable acc = this.Accelerate
    let mutable pos = this.Position
    let mutable time = 1.0
    if simtime <= 365 && interval < 2 then
      for i in 1 .. simtime*10 do
        pos  <- (first pos + (first vel * time), second pos + (second vel * time), third pos + (third vel * time)) 
        vel  <- (first vel + (first acc * time), second vel + (second acc * time), third vel + (third acc * time))
        acc  <- (this.AccEq(pos) * first pos, this.AccEq(pos) * second pos, this.AccEq(pos) * third pos) 
        planetList <- planetList@[(first pos, second pos)]
    elif simtime <= 365 && interval = 30 then
      for i in 1 .. simtime*10 do
        pos  <- (first pos + (first vel * time), second pos + (second vel * time), third pos + (third vel * time)) 
        vel  <- (first vel + (first acc * time), second vel + (second acc * time), third vel + (third acc * time))
        acc  <- (this.AccEq(pos) * first pos, this.AccEq(pos) * second pos, this.AccEq(pos) * third pos) 
        planetList <- planetList@[(first pos, second pos)]
        if i = 1 || i%(interval*10) = 0 && simtime - interval < i then
          printfn "%A" pos
    else
      printfn "invalid simulation or interval time, maximum is 365 days and 30 day interval. Please try again:"
      this.Simulate (int (System.Console.ReadLine())) (int (System.Console.ReadLine()))
  //Her tages der højde for, om den liste a koordinater er tom. 
  //Er dette tilfældet, anvender vi List.map, for at gemmme koordinater,
  //for at bibevare listen af koordinater. 
    if temp = [] then
      temp <- planetList |> List.map (fun x -> [x])
  //Hvis temp ikke er tom, sammensættes listen af planeternes positioner 
  //sammen med den overordnede liste (temp). Dernæst bliver alle lister
  //sammensat i den samme overordnede liste.
    else
      temp <- List.zip planetList temp |> List.map (fun (posPlanet, posOtherPlanets) -> posPlanet :: posOtherPlanets)
    planetColors <- color :: planetColors
    names <- filename :: names

  //Printer planeters position for hver 30. dag i 2016. 
  member this.NasaData =  
    let mutable k = 91311
    for i in 1..12 do
      longData <- float arraydata.[k]
      latData  <- float arraydata.[k+1] + 90.0
      radData  <- float arraydata.[k+2] 
      printfn "Month: %i %A" i this.DataPosition
      k <- k + 150

end

//Pluto er ikke en planet.
type DwarfPlanet(filename:string) = class
  
  inherit Planet(filename, Brushes.Gray)
  
  let first (a, _, _) = a
  let second (_, b, _) = b
  let third (_, _, c) = c

  //let AU = 149597870700 //m
  //let GravityK = 6.67384 * (10.0**(-11.0)) //m³/(s²*kg)
  let GMsun = 2.959122082322128 * (10.0**(-4.0)) //AU³/day²
  let convert = Math.PI/180.0
  
  //Her åbnes den valgte datafil (for en planet)
  let read = System.IO.File.OpenText filename
  //Fjerner tomme pladser i strengen
  let removeEmpty = StringSplitOptions.RemoveEmptyEntries
  let fulltext = read.ReadToEnd()
  //specificerer det indeks, hvor vi starter med at læse filen.
  let startIndex = fulltext.IndexOf("$$SOE") + 5
  //specificerer det indeks, hvor vi skal slutte med at læse filen.
  let endIndex = fulltext.LastIndexOf("$$EOE") - 5
  let usabletext = fulltext.[startIndex..endIndex]
  let arraydata = usabletext.Split ([|' '; '\r'|], removeEmpty) 
  
  let long = float arraydata.[91311]
  let lat = float arraydata.[91312] + 90.0
  let rad = float arraydata.[91313]

  let sndlong = float arraydata.[91316]
  let sndlat = float arraydata.[91317] + 90.0
  let sndrad = float arraydata.[91318]

  let mutable longData = 0.0
  let mutable latData = 0.0
  let mutable radData = 0.0
  
end

let mutable i = 0

// Function to create the window
let createForm backgroundColor (width, height) title draw =
  let win = new Form ()
  win.Text <- title
  win.BackColor <- backgroundColor
  win.ClientSize <- Size (width, height)
  win.Paint.Add draw
  win

// Properties of the window
let title = "Solar System"  
let backgroundColor = Color.Black
let size = (800, 800)
let pen = (Color.White, 1.0)


//Definerer hvilken skrifttype og størrelse
let font = new Font("Arial", 9.0F)
//Her er det speicelt, hvor List.zip3 sammensætter 3 listers værdier. 
//for at kunne give en planet et navn, farve, og positioner. 
let drawPoints  (i : int byref) (e : PaintEventArgs) =
  if i < temp.Length then
    for (pen, name, planet) in List.zip3 planetColors names temp.[i] do
      let (x, y) = planet
      e.Graphics.FillEllipse (pen, int ((x*10.0)+400.0), int ((y*10.0)+400.0),10,10)
      e.Graphics.DrawString(name, font, Brushes.White, float32 ((x*10.0)+400.0), float32 ((y*10.0)+400.0))
    let size = 20
    let pen = new Pen (Color.Black)
    let brush = new SolidBrush (Color.Yellow)
    e.Graphics.DrawEllipse (pen, 400, 400, size, size)
    e.Graphics.FillEllipse (brush, 400, 400, size, size)
  

// Function for updating the center point of the box
let updatePoints (form : Form) showtime =
  if i < (temp.Length - 1) then
    i <- i + 1
    form.Refresh()
    printfn "%A" temp.[i]

let main() =
  // Create window
  let win = createForm backgroundColor size title (drawPoints &i)
  
  let Mercury = new Planet ("Mercury.txt", Brushes.Brown)
  let Venus = new Planet ("Venus.txt", Brushes.Orange)
  let Earth = new Planet ("Earth.txt", Brushes.Green)
  let Mars = new Planet ("Mars.txt", Brushes.Red)
  let Jupiter = new Planet ("Jupiter.txt", Brushes.OliveDrab)
  let Saturn = new Planet ("Saturn.txt", Brushes.Pink)
  let Uranus = new Planet ("Uranus.txt", Brushes.Purple)
  let Neptune = new Planet ("Neptune.txt", Brushes.Blue)
  let Pluto = new Planet ("Pluto.txt", Brushes.Gray)

  Mercury.Simulate 365 1
  Venus.Simulate 365 1
  Earth.Simulate 365 1
  Mars.Simulate 365 1
  Jupiter.Simulate 365 1
  Saturn.Simulate 365 1
  Uranus.Simulate 365 1
  Neptune.Simulate 365 1
  Pluto.Simulate 365 1

  // Create timer
  let timer = new Timer()
  timer.Interval <- 100
  timer.Enabled <- true
  let dtheta = 0.01
  timer.Tick.Add (updatePoints win)
  
  // Application.Run win
  Application.Run win
  
  printfn "* MercuryTest *"
  Mercury.Simulate 365 30
  printfn ""
  Mercury.NasaData
  printfn "* VenusTest *"
  Venus.Simulate 365 30
  printfn ""
  Venus.NasaData
  printfn "* EarthTest *"
  Earth.Simulate 365 30
  printfn ""
  Earth.NasaData
  printfn "* MarsTest *"
  Mars.Simulate 365 30
  printfn ""
  Mars.NasaData
  printfn "* JupiterTest *"
  Jupiter.Simulate 365 30
  printfn ""
  Jupiter.NasaData
  printfn "* SaturnTest *"
  Saturn.Simulate 365 30
  printfn ""
  Saturn.NasaData
  printfn "* UranusTest *"
  Uranus.Simulate 365 30
  printfn ""
  Uranus.NasaData
  printfn "* NeptuneTest *"
  Neptune.Simulate 365 30
  printfn ""
  Neptune.NasaData
  printfn "* PlutoTest *"
  Pluto.Simulate 365 30
  printfn ""
  Pluto.NasaData

main()