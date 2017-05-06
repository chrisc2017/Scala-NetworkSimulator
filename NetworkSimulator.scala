package ScalaNetworkSimulator

import scala.language.implicitConversions
import scala.collection.mutable
import scala.io.StdIn.{readLine}//this allows us to read from the command line <-----used in the object 'config'

class NetworkSimulator {
  
  // These are references we will keep in order to keep track of what devices we are setting up
  var portRef: PortClass = null
  var deviceRef: AnyRef = null
  var protocolRef: RoutingProtocolClass = null
  var linkRef: LinkClass = null
  var routerRef: RouterClass = null
  var currentDevice: AnyRef = null

  // global tables 
  var globalDeviceTable: mutable.HashMap[String, AnyRef] = new mutable.HashMap[String, AnyRef]()
  var globalProtocolTable: mutable.HashMap[String, RoutingProtocolClass] = new mutable.HashMap[String, RoutingProtocolClass]()
  var globalLinksTable: mutable.HashMap[PortClass, PortClass] = new mutable.HashMap[PortClass, PortClass]()
  var globalMACaddressCheck: mutable.HashMap[String, MACAddress] = new mutable.HashMap[String, MACAddress]()
  var globalIPaddressCheck: mutable.HashSet[String] = new mutable.HashSet[String]()
  var globalRouterList: mutable.ArrayBuffer[RouterClass] = new mutable.ArrayBuffer[RouterClass]()
  var globalPortTypeTable: mutable.HashMap[String, PortTypeClass] = new mutable.HashMap[String, PortTypeClass]()
  
  // MAGIC NUMBERS
  var sec1 = 1
  var sec2 = 2
  
  globalPortTypeTable += ("Fiber" -> new PortTypeClass("Fiber", 100, 100))
  globalPortTypeTable += ("Ethernet" -> new PortTypeClass("Ethernet", 10, 10))
  
  // The RoutingProtocol object parses the commands (brackets represent optional values. Parenthesis for variables)
  // RoutingProtocol name (String)
  object RoutingProtocol {
    
    def name(name: String) = {
      val protocol = new RoutingProtocolClass(name)
      // Set our protocolRefRef reference to this class
      // Next time we call our Port object we will know to add the port to this device
      protocolRef = protocol
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalProtocolTable.contains(name)) {
        println("You have already defined a routing protocol named " + name + ".\n Please rename your routing protocols so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalProtocolTable += (name -> protocolRef)
      }
    }
  }
  
  // Helper objects for RoutingProtocol. We can access the RoutingProtocolClass object using our protocolRef reference
  
  // The learn object will set what values the RoutingProtocolClass should use when the Router's are first ran and start learning the network
  // Learn object parses the command
  // learn by (String). The string can be only support bandwith, speed, both
  object learn {
    def by(value: String) = {
      protocolRef.learn = value
    }
  }
  
  // The choose object will set what values the RoutingProtocolClass should use when the Router chooses which port to send data
  // choose object parses the command
  // choose by (String). The string can be only support min, max, avg
  object choose {
    def port(choose: String) = {
      protocolRef.choose = choose
    }
  }
  

  
  
  // The Switch object parses the commands (brackets represent optional values. Parenthesis for variables)
  // Switch name (String)
  object Switch {
    
    def name(name: String) = {
      val device = new SwitchClass(name)
      // Set our deviceRef reference to this class
      // Next time we call our Port object we will know to add the port to this device
      deviceRef = device
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalDeviceTable.contains(name)) {
        println("You have already defined a device named " + name + ".\n Please rename your devices so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalDeviceTable += (name -> deviceRef)
      }
    }
    
  }
  
  // The PC object parses the commands (brackets represent optional values. Parenthesis for variables)
  // PC name (String)
  object PC {
    
    def name(name: String) = {
      val device = new PCClass(name);
      
      // Set our deviceRef reference to this class
      // Next time we call our Port object we will know to add the port to this device
      deviceRef = device
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalDeviceTable.contains(name)) {
        println("You have already defined a device named " + name + ".\n Please rename your devices so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalDeviceTable += (name -> device)
      }
    }
    
  }
  
  // The Router object parses the commands (brackets represent optional values. Parenthesis for variables)
  // Router name (String)
  object Router {
    
    def name(name: String) = {
      val device = new RouterClass(name);
      
      // Set our deviceRef reference to this class
      // Next time we call our Port object we will know to add the port to this device
      deviceRef = device
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalDeviceTable.contains(name)) {
        println("You have already defined a device named " + name + ".\n Please rename your devices so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalDeviceTable += (name -> device)
        globalRouterList += device
      }
    }
    
  }
  
  // Helper objects for Router
  
  
  // The routing object will select what routing protocol the router should use
  // routing object parses the command
  // routing protocol (String). 
  object routing {
    
    def protocol(name: String) = {
      
      val router = deviceRef.asInstanceOf[RouterClass]
      router.protocol = globalProtocolTable.getOrElse(name, sys.error("You are trying to assign a router to routing protocol " + name + "which does not exist.\nPlease check your configuration file."))
    }
  }
  
  
  // The Port object parses the commands (brackets represent optional values. Parenthesis for variables)
  // port num (Int) [uses (String)] [IPaddress (String)]
  object port{
    
    def num(portNum: Int): this.type = {
      
      // Create new the PortClass object
      val port = new PortClass(portNum)
      
      
      // Add PortClass object to whatever Device we created (Switch,Router,PC)
      // The reason we invoke a method in this instance is because we know that SwitchClass, RouterClass
      //   and PCClass all have the method addPort.
      deviceRef.getClass().getMethod("addPort", classOf[PortClass]).invoke(deviceRef, port)
      port.device = deviceRef
      // sets portRef to current PortClass we created
      portRef = port
      
      return this
    }
    
    def uses(portType: String): this.type = {
      //All we have to do is set the PortType but we need to instantiate Fiber, Ethernet, etc first.
      portRef.portType = globalPortTypeTable.get(portType).get
      return this
    }
    
    def IPAddress(IPAddr: String) = {
      /* Can do checks here to make
      if (deviceRef.getClass() == classOf[SwitchClass]) {
        print("error: you cannot assign an IP Address to a switch port")
        exit(0)
      }*/
      
      globalIPaddressCheck += IPAddr
      portRef.IPAddr = IPAddr
      portRef.MACAddr = new MACAddress()
      portRef.MACAddr.makeMAC(IPAddr)
      
    }
    
  }
  
  object default {
    
    def gateway(IPAddr: String) = {
      if (deviceRef.isInstanceOf[PCClass]) {
        deviceRef.asInstanceOf[PCClass].defaultGateway = IPAddr
      }
    }
  }
  
  
  // Links between devices
  // We only use this object as a syntax requirement (meaning you have to specify your Links; section in the configuration)
  object Links {
    // Don't really need to do anything here
  }
  
  // links helper methods
  
  // The connect object is what we are using to instantitate LinkClass objects
  // The connect object parses the commands (brackets represent optional values. Parenthesis for variables)
  // connect deviceA (String) portA (Int) deviceB (String) portB (Int)
  object connect {
    
    //entry point of our connect command
    def deviceA(name: String): this.type = {
      val link = new LinkClass()
      link.deviceA = globalDeviceTable.getOrElse(name, sys.error("Device A (named: " + name + ") does not exist. Please change the name of deviceA"))
      linkRef = link
      return this
    }
    
    def portA(num: Int): this.type = {
      
      if (linkRef.deviceA.isInstanceOf[SwitchClass]) {
        linkRef.portA = linkRef.deviceA.asInstanceOf[SwitchClass].ports.getOrElse(num, sys.error("Device A does not have port number " + num.toString + "."))
      }
      else if  (linkRef.deviceA.isInstanceOf[RouterClass]) {
        linkRef.portA = linkRef.deviceA.asInstanceOf[RouterClass].ports.getOrElse(num, sys.error("Device A does not have port number " + num.toString + "."))
      }
      else {
        if (linkRef.deviceA.asInstanceOf[PCClass].port.num != num) {
          println("Device A does not have port number " + num.toString + ".")
          sys.exit(1)
        }
        linkRef.portA = linkRef.deviceA.asInstanceOf[PCClass].port
      }
      return this
    }
    
    def deviceB(name: String): this.type = {
      linkRef.deviceB = globalDeviceTable.getOrElse(name, sys.error("Device B (named: " + name + ") does not exist. Please change the name of deviceA"))
      return this
    }
    
    def portB(num: Int): this.type = {
      
      if (linkRef.deviceB.isInstanceOf[SwitchClass]) {
        linkRef.portB = linkRef.deviceB.asInstanceOf[SwitchClass].ports.getOrElse(num, sys.error("Device A does not have port number " + num.toString + "."))
      }
      else if  (linkRef.deviceB.isInstanceOf[RouterClass]) {
        linkRef.portB = linkRef.deviceB.asInstanceOf[RouterClass].ports.getOrElse(num, sys.error("Device A does not have port number " + num.toString + "."))
      }
      else {
        if (linkRef.deviceB.asInstanceOf[PCClass].port.num != num) {
          println("Device A does not have port number " + num.toString + ".")
          sys.exit(1)
        }
        linkRef.portB = linkRef.deviceB.asInstanceOf[PCClass].port
      }
      
      if (linkRef.portA.portType != linkRef.portB.portType) {
        println("You cannot connect two ports that are different types. Please modify your configuration.")
          sys.exit(1)
      }
      
      globalLinksTable += (linkRef.portA -> linkRef.portB)
      globalLinksTable += (linkRef.portB -> linkRef.portA)
      return this
    }
  }
  
  //this method will find matching destination mac address for the known destination IP address
  //this request can be sent from Routers or PC's
  def requestARP(pdu: PDU){
    
    if( pdu.packet(8).isInstanceOf[SwitchClass] ){//if the current device the packet is on is a switch
  
      for(counter <- 1 to pdu.packet(8).asInstanceOf[SwitchClass].ports.size ){
        
        if(pdu.packet(8).asInstanceOf[SwitchClass].ports.get(counter) != pdu.packet(7).asInstanceOf[PortClass] ){
          
          //update PDU(5) and PDU(6) to the port# and device ref of the port on the opposite side of the Link
          pdu.packet(7) = globalLinksTable.get( pdu.packet(8).asInstanceOf[SwitchClass].ports.get(counter).get )//set next port on link as the current port
          pdu.packet(8) = globalLinksTable.get( pdu.packet(8).asInstanceOf[SwitchClass].ports.get(counter).get ).get.device //set next port's device as the current device
          println("forwarding ARP request out of port " + counter.toString())
          requestARP(pdu)//causes recursive call to flood all switch ports with an ARP request
        }
        //do nothing if the pdu(5) == the counter # <---- we dont want to forward ARPrequest back to the devices that sent the original ARPrequest
      }//end of loop
    
    }//end of dealing with switches
    
    
    //if we get here in the code then we have either reached a PC or a router
    //check if this PC's port has the IP address of the packet's destination IP
    if( pdu.packet(7).asInstanceOf[PortClass].IPAddr == pdu.packet(1) ){
          println(pdu.packet(8).asInstanceOf[PCClass].name + " successfully received the ARP request. Sending reply to "  )
    }
    else{
      println("Dropping the packet because the destination IP address does not match its IP address")
    }
    
    
    
  }//end of requestARP
  
  
  
  def sendPDU(pdu: PDU){
    
    var hopCount: Int = 0
    var deviceName: String = "not yet queried"
    //var currentPortIP: String = "not yet set" <----- do not need
    var timeCount: Int = 0
    
    while( pdu.packet(7).asInstanceOf[PortClass].IPAddr != pdu.packet(1) ){//while the packet has not arrived at the correct port 
      
      if( pdu.packet(4) == "traceroute" && !pdu.packet(8).isInstanceOf[SwitchClass] ){//if this is a traceroute being sent
        
        hopCount += 1
        timeCount += pdu.packet(7).asInstanceOf[PortClass].portType.speed/60
        
        if( pdu.packet(8).isInstanceOf[PCClass]  ){//if the current device reference is a PC
          deviceName = pdu.packet(8).asInstanceOf[PCClass].name
        }
        else{//if the current device reference is a Router
          deviceName = pdu.packet(8).asInstanceOf[RouterClass].name
        }
        println(hopCount + " " + deviceName + " (" + pdu.packet(7).asInstanceOf[PortClass].IPAddr + ") " + timeCount)
      }//end of traceroute
        
      
      //now update the current device port and current device reference <-----this is how we move the packet from port to port across the network
      if( pdu.packet(8).isInstanceOf[PCClass] ){//if this device is a PC
        
        pdu.packet(7) = globalLinksTable.get( pdu.packet(7).asInstanceOf[PortClass] ).get//updated the packet port reference to the port on the other side of the Link
        pdu.packet(8) = pdu.packet(7).asInstanceOf[PortClass].device//updated the packet device reference to the new port's assigned device
        
      }else if( pdu.packet(8).isInstanceOf[SwitchClass] ){//if the device is a switch
        
        pdu.packet(7) = pdu.packet(8).asInstanceOf[SwitchClass].MACaddrTable.get(pdu.packet(3).asInstanceOf[String]).get //this queries the switch's MAC address table to produce the outgoing (next) port we need
        pdu.packet(7) = globalLinksTable.get( pdu.packet(7).asInstanceOf[PortClass] )//since we are already on the outgoing port of this switch we might as well move to the next device instead of wasting another iteration of this while loop
        pdu.packet(8) = pdu.packet(7).asInstanceOf[PortClass].device//updated the packet device reference to the new port's assigned device
      }
      else{//if this device is a router
        pdu.packet(7) = pdu.packet(8).asInstanceOf[RouterClass].RoutingTable.get( pdu.packet(1).asInstanceOf[IPAddress].ipAddressValue ) //this quries the routing table for the best route that matches this IP address
        pdu.packet(7) = globalLinksTable.get( pdu.packet(7).asInstanceOf[PortClass] )//since we are on the exit port of this router we might as well move the packet to the next device
        pdu.packet(8) = pdu.packet(7).asInstanceOf[PortClass].device//updated the packet device reference to the new port's assigned device
      }
      
    }//end while
    
    //if we get here we have reached out destination
    if( pdu.packet(8).isInstanceOf[PCClass] ){//if this device is a PC
      pdu.packet(8).asInstanceOf[PCClass].checkIncomingDestMac(pdu)
    }
    else if( pdu.packet(8).isInstanceOf[RouterClass] ){//if this device is a Router
      
      if( pdu.packet(4) == "ping" ){
        println("64 bytes from (" + pdu.packet(1).asInstanceOf[IPAddress].ipAddressValue + ")    time = " + timeCount)
      }
      else if(pdu.packet(4) == "traceroute"){
        println(hopCount + " " + deviceName + " (" + pdu.packet(7).asInstanceOf[PortClass].IPAddr + ") " + timeCount)
      }
      else{
        println("Device " + pdu.packet(8).asInstanceOf[RouterClass].name + " is dropping packet because this command " + pdu.packet(4) + " is not recognized.")

      }
      
    }
    else{//there is not reason other than testing or a mistake in the code to reach a switch as your PDU's final destination -----> Note: we do not current provide management IP's for switches
      println("Device " + pdu.packet(8).asInstanceOf[SwitchClass].name + " is dropping packet.")
    }
}
  
  
  //this object builds the network
  /*
   * theory: as each line of the main method is read in via the scala main method the individual objects create network
   * devices, their properties, and the links between them. 
   * 
   * This object call is written at the end of the main method code so that it launches the CLI.
   * 
   * the purpose of the config object is provide the user access to a CLI environment so that interactions with the 
   * simulated network can happen
   */
  object Config{
    
    var s: String = ""
    var input: String = ""
    
    /*commands the user will have available:
    *
    * 1. exit -> this ends the cli and exits the simulation
    * 
    * 2. changeDevice <device name> -> this changes the current device referenced from one device to another.  
    * 
    * 3. send <IP address> <command: storgeThis | replyWith> <String of char>-> sends a Protocol Data Unit across the network to another IP. If the correct PC device IP receives the data that device prints the data to the CLI. Upon failure -> print device unreachable.
    * 
    * 4. ping <IP> -> this will send a PDU to the specified IP (any device's IP) but it will not print its path as it goes. 
    * It will only print "xx bytes from <destination IP>: time= <number of seconds the links specify added together> ms".
    * Upon failure print "device not reachable"
    *
    * 5. traceroute <IP> -> this acts just like ping, but it will only print "<counter of hop> <device name> <IP of port> time=<cummulative time in seconds that it took to get here based on link times> ms"
    * when it hits a port that has an IP address. Upon failure print "device unreachable"
    * 
    * 6. inspect [mactable | arptable | routingtable]	 -> this allows the user to see the device's specified table entries. It the table does not exits then print "Table does not exits on this type of device."
    * If table does exist this command will print out the entries to the command line.
    * 
    * Upon parsing failure of any of these commands -> print "Error: Command spelling or incorrect amount of arguments. Please see command help"
    * 
    * 7. Help just prints to the CLI a list of the command prototypes and a simple explination
    * 
    * */
    
    // Entry point to our simulation
    def Network() = {
      runCLI
    }
    
    def routerLearn(port: PortClass){
      // Gets the port from the opposite end of our port
      Thread.sleep(sec1)
      var oppPort: PortClass = globalLinksTable.get(port).get
      if (oppPort.device.isInstanceOf[SwitchClass]) {
        if (protocolRef == null) {
          routerRef.currentWeight += port.portType.speed
        }
        else if (protocolRef.learn == "bandwith") {
          routerRef.currentWeight += port.portType.bandwith
        }
        else {
          routerRef.currentWeight += port.portType.speed
        }
        
        var device = oppPort.device.asInstanceOf[SwitchClass]
        device.portReceived = oppPort.num
        println(device.devType + " " + device.name + " received a packet at port " + oppPort.num +". Switches just need to forward the packets. Sending packet out of the rest of it's ports.")
        
        for (port <- device.ports.values) {
          if (port.num != device.portReceived) {
            
            println("Sending packet out of port " + port.num.toString + ".\n")
            routerLearn(port)
          }
        }
      }
      else if (oppPort.device.isInstanceOf[RouterClass]) {
        var device = oppPort.device.asInstanceOf[RouterClass]
        device.portReceived = oppPort.num
        println(device.devType + " " + device.name + " received a packet at port " + oppPort.num +". Router will send it's information it currently has.")
        for (route <- device.RoutingTable) {
          if (!routerRef.RoutingTable.contains(route._1)) {
            val map = mutable.HashMap[Int, PortClass]()
            val weight = routerRef.currentWeight
            if (protocolRef == null) {
              routerRef.currentWeight += oppPort.portType.speed
            }
            else if (protocolRef.learn == "bandwith") {
              routerRef.currentWeight += oppPort.portType.bandwith
            }
            else {
              routerRef.currentWeight += oppPort.portType.speed
            }
            println("Added IP Address " + route._1 + " to routing table with weight " + weight + " through port " + routerRef.portSent.num)
            map += (weight -> routerRef.portSent)
            routerRef.RoutingTable.put(route._1, map)
          }
        }
        
        for (port <- device.ports.values) {
          if (port.num != device.portReceived) {
            
            println("Sending packet out of port " + port.num.toString + ".\n")
            routerLearn(port)
          }
        }
      }
      else {
        var device = oppPort.device.asInstanceOf[PCClass]
        device.portReceived = oppPort.num
        println(device.devType + " " + device.name + " received a packet at port " + oppPort.num +". PCs will drop the packets because they provide no information.")
      }
    }
    //this runs the CLI -> this method is only called by the main method in ScalaNetworkSimulator
    def runCLI{
      //user must select a device to start with:
      var deviceRef: AnyRef = null
      var splitStringArray:Array[String] = new Array[String](4)
      
      // Entry point into our simulation
      println("************************************************************************************************************************\n************************************************************************************************************************")
      println("\nSystem booted up. Devices are powering on...\n")
      println("************************************************************************************************************************\n************************************************************************************************************************")
      Thread.sleep(sec2)
      
      // Learning phase
      // This is when router's will try to learn information about the device in the network.
    
      
      for (router <- globalRouterList) {
        println("\nRouter " + router.name + " booted up.")
        Thread.sleep(sec1)
        router.learnDirectlyConnectedRoutes
      }
      
            
      println("\nRouters are powering on. Routers will send data through all ports to learn about it's network.\n")
      println("************************************************************************************************************************\n************************************************************************************************************************")
      Thread.sleep(sec2)  
      
      for (router <- globalRouterList) {
        
        routerRef = router
        println("************************************************************************************************************************\n************************************************************************************************************************")
        println("\nLearning phase for router " + router.name + " beginning.")
        println("Router currently has " + router.RoutingTable.size + " IP Addresses in it's routing table. (One for each connected port)")
        Thread.sleep(sec1)
        println("\nStarting to send packets out of all ports.....")
        Thread.sleep(sec2)
        for (port <- router.ports.values) {
          println("\nSending packet out of port " + port.num.toString + ".")
          router.portSent = port
          router.currentWeight = 0
          routerLearn(port)
        }
        println("************************************************************************************************************************\n************************************************************************************************************************")        
        println("Router " + router.name + " has finished learning about the network.")
        println("Router now has " + router.RoutingTable.size + " IP Addresses in it's routing table.")
        println("************************************************************************************************************************\n************************************************************************************************************************")

      }
      
      println("Running simulator...")
      Thread.sleep(sec2)
      println("Welcome to Scala Network Simulator. Please choose a device to get started or type help.")
      
      
      while( s != "exit" ){
           if (currentDevice.isInstanceOf[PCClass]) {
             print(currentDevice.asInstanceOf[PCClass].name)
           }
           else if (currentDevice.isInstanceOf[RouterClass]) {
             print(currentDevice.asInstanceOf[RouterClass].name)
           }
           else if (currentDevice.isInstanceOf[SwitchClass]) {
             print(currentDevice.asInstanceOf[SwitchClass].name)
           }
           else {
             print("Admin")
           }
           print("> ")
           s = readLine(input)//provides the user with a prompt
           splitStringArray = s.split(" ") //think about error checking the number of arguments later
           
           if(splitStringArray(0) == "ping" ){
             if (!currentDevice.isInstanceOf[PCClass]) {
               println("You cannot send data from a Router or Switch. Please change to a PC and try to send again.")
             }
             else {
               //ping( splitStringArray(1) ) //send command will call sendPDU
               if (!globalIPaddressCheck.contains(splitStringArray(1))) {
                 println("Pinging " + splitStringArray(1) + " from of " + currentDevice.asInstanceOf[PCClass].name)
                 println("This IP Address does not exist on your network. Please try another IP Address")
               }
               else {
                 ping(splitStringArray(1))
               }
             }
           }
           else if(splitStringArray(0) == "traceroute" ){
             if (!currentDevice.isInstanceOf[PCClass]) {
               println("You cannot send data from a Router or Switch. Please change to a PC and try to send again.")
             }
             else {
               //ping( splitStringArray(1) ) //send command will call sendPDU
               if (!globalIPaddressCheck.contains(splitStringArray(1))) {
                 println("Traceroute on " + splitStringArray(1) + " from of " + currentDevice.asInstanceOf[PCClass].name)
                 println("This IP Address does not exist on your network. Please try another IP Address")
               }
               else {
                 traceroute(splitStringArray(1))
               }
             }
           }
           else if(splitStringArray(0) == "send" ){
             if (!currentDevice.isInstanceOf[PCClass]) {
               println("You cannot send data from a Router or Switch. Please change to a PC and try to send again.")
             }
             else {
               send( splitStringArray(1), splitStringArray(2), splitStringArray(3) ) //send command will call sendPDU
             }
           }
           else if(splitStringArray(0) == "inspect" ){
             inspect
           }
           else if(splitStringArray(0) == "changeDevice" ){
             changeDevice( splitStringArray(1) )
           }
           else if(splitStringArray(0) == "help" ){
             help //is this how we run a stand alone command with no arguments?
           }
           else if(splitStringArray(0) == "exit" ){
             s = "exit"
           }
            
        } 
      
      println("Thank you for trying the Scala Network Simulator")
    }
    
    
    
    def ping(inputIP: String){
      
      if (currentDevice.isInstanceOf[PCClass]) {
        var weight = 0
        for (device <- globalDeviceTable.values) {
          weight += 100
          if (device.isInstanceOf[PCClass] && device.asInstanceOf[PCClass].port.IPAddr == inputIP) {
            println("from " + device.asInstanceOf[PCClass].name + " " + inputIP + " time=" + weight + " ms.")
          }
        }
      }
      else {
        println("You must ping from a PC.")
      }

    }
    
    def traceroute(inputIP: String){
      
      if (currentDevice.isInstanceOf[PCClass]) {
        var weight = 0
        for (device <- globalDeviceTable.values) {
          if (device.isInstanceOf[RouterClass]) {  
            weight += 100
            println(device.asInstanceOf[RouterClass].devType + " " + device.asInstanceOf[RouterClass].name + " " + device.asInstanceOf[RouterClass].ports.head._2.IPAddr + " " + weight + "ms.")
          }
        }
        for (device <- globalDeviceTable.values) {
          weight += 100
          if (device.isInstanceOf[PCClass] && device.asInstanceOf[PCClass].port.IPAddr == inputIP) {
            println("from " + device.asInstanceOf[PCClass].name + " " + inputIP + " time=" + weight + " ms.")
          }
        }
      }
      else {
        println("You must ping from a PC.")
      }
      
    }
    
    
    def send(inputIP: String, fileFormat: String, data: String){
      
      
      if (currentDevice.isInstanceOf[PCClass]) {
        var weight = 0
        for (device <- globalDeviceTable.values) {
          weight += 100
          if (device.isInstanceOf[PCClass] && device.asInstanceOf[PCClass].port.IPAddr == inputIP) {
            println("from " + device.asInstanceOf[PCClass].name + " storing file" + fileFormat )
            device.asInstanceOf[PCClass].assignStorage( fileFormat, data)
          }
        }
      }
      else {
        println("You must send from a PC.")
      }

    }
    
    
    def inspect{
      //inspect [mactable | arptable | routingtable]
      
      
      if( currentDevice.isInstanceOf[PCClass]){
        println("All data currently in the PC's ARP table.")
          for (data <- currentDevice.asInstanceOf[PCClass].ARPTable) {
            println(data._1)
            println(data._2)
          }
        println("All data currently stored in PC")
        for (data <- currentDevice.asInstanceOf[PCClass].storage.values) {
            println(data)
          }
      }
      else if( currentDevice.isInstanceOf[SwitchClass]){
        println("All data currently in the switch's MAC Address table.")
        for (data <- currentDevice.asInstanceOf[SwitchClass].MACaddrTable) {
          println(data._1)
          println(data._2)
        }
      }
      else if( currentDevice.isInstanceOf[RouterClass]){
        println("All data currently in the router's ARP table.")
        for (data <- currentDevice.asInstanceOf[RouterClass].ARPTable) {
          println(data._1)
          println(data._2)
        }
        println("All data currently in the router's routing table.")
        for (data <- currentDevice.asInstanceOf[RouterClass].RoutingTable) {
          println(data._1)
          println(data._2)
        }
      }
      else{
          println("You are currently an Admin. Please change to a device using the keyword changeDevice <device name>")
      }
      
    }
    
    
    
    
    def changeDevice( inputName: String){
      if( globalDeviceTable.contains(inputName) ){
        currentDevice = globalDeviceTable.get(inputName).get
        if (currentDevice.isInstanceOf[PCClass]) {
          println("Found " + currentDevice.asInstanceOf[PCClass].devType + " " + currentDevice.asInstanceOf[PCClass].name + ". Switching to current device.")
        }
        else if (currentDevice.isInstanceOf[RouterClass]) {
          println("Found " + currentDevice.asInstanceOf[RouterClass].devType + " " + currentDevice.asInstanceOf[RouterClass].name + ". Switching to current device.")
        }
        else {
          println("Found " + currentDevice.asInstanceOf[SwitchClass].devType + " " + currentDevice.asInstanceOf[SwitchClass].name + ". Switching to current device.")
        }
      }
      else{
          println("Device does not exist in simulation. Please check your spelling and try again. ")
      }
      
    }
    
    
    
    //prints to CLI the available commands
    def help(){
      println("------------------------------------------------available commands------------------------------------------------")
      println()
      println("exit		                                                        -> Ends the simulation")
      println()
      println("changedevice <device name>	                                   	-> Changes to another device's CLI.")
      println()
      println("send <IP address> <format> <String of char without spaces>              -> sends data specified as a String without spaces across the network to another IP.")
      println()
      println("ping <IP>		                                                -> Checks the availability of an IP address.")
      println()
      println("traceroute <IP>		                                                -> Prints the path from the current device to the specified IP address.")
      println()
      println("inspect [mactable | arptable | routingtable]		                -> Prints the contents of the specified table from the current device.")
      println()
    }
  }
  
  
  
  
}//end network simulator