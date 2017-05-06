package ScalaNetworkSimulator
import scala.collection.mutable


/*
class NetworkClasses {
  
}
*/

class SwitchClass(pname: String) {
  var name = pname
  var devType = "Switch"
  var ports = new mutable.HashMap[Int, PortClass]()
  var portReceived = -1
  var MACaddrTable = new mutable.HashMap[String, PortClass]()
  
  
  def addPort(port: PortClass) = {
    // Check if port already exists in our object. If yes, give error and exit. Else, add to ports
      if (ports.contains(port.num)) {
        println("You have already defined a port at port" + port.num + ".\n Please rename your port numbers.")
        System.exit(0)
      }
      else {
        port.device = this
        // appends the newly added port to our ports ArrayBufferz
        ports += (port.num -> port)
      }
  }
  
  //method for updateing/learning new mac addr/port combinations <-----run everytime the switch 
  def update(inputMAC: String, inputPort: PortClass){
    
    if(!MACaddrTable.contains(inputMAC) ) {
      MACaddrTable += (inputMAC -> inputPort)//add the new entry to the table
    }
  }
    
}



class RoutingProtocolClass(name: String) {
  var learn = ""
  var choose = ""
  
  //cost -> how many hop
  
}

class RouterClass(pname: String) {
  var name = pname
  var devType = "Router"
  var ports = new mutable.HashMap[Int, PortClass]()
  var portReceived = -1
  var portSent: PortClass = null
  var currentWeight = 0
  var protocol: RoutingProtocolClass = null
  var ARPTable = new mutable.HashMap[String, String]()//stores map of IP address -> mac address (allows us to need fewer arp requests as the simulation progresses)
  var RoutingTable = new mutable.HashMap[String, mutable.HashMap[Int, PortClass]]()// stores map of port# -> IP address (allows us to send data out the correct port)


  def addPort(port: PortClass) = {
    // Check if port already exists in our object. If yes, give error and exit. Else, add to ports
      if (ports.contains(port.num)) {
        println("You have already defined a port at port" + port.num + ".\n Please rename your port numbers.")
        System.exit(0)
      }
      else {
        port.device = this
        // appends the newly added port to our ports ArrayBufferz
        ports += (port.num -> port)
      }
  }
      
  //learn mac address/port combinations
  //method for updateing/learning new mac addr/port combinations <-----run everytime the switch 
  def update(inputIP: String, inputMAC: String){
    
    if(!ARPTable.contains(inputMAC)) {
      ARPTable += (inputIP -> inputMAC)//add the new entry to the table
    }
  }    
  
   //learn directly connected routes after config phase, but before simulation start
  def learnDirectlyConnectedRoutes{
    
    for (port <- ports.values){
      println("Router " + this.name + " has learned about port " + port.num + " with IP Address " + port.IPAddr)
      val map = mutable.HashMap[Int, PortClass]()
      map += ( port.portType.speed -> port)
      RoutingTable += ( port.IPAddr -> map)
    }//end loop
    
  }
      
   //learn static routes <-----user specifies a route for the routing table (can be added later)
  /*
   * input statement just like a link -> 'staticRoute (destingation IP) (exit port on router)' <----user straight up tells the router to send data that matches this ip to be sent out this port
   * 
   * 
   *     
   */
  
   //learn dynamic routes <----- routing protocols shared from other connected routers send updates (can be added later)
      
      
      
  
}



class PCClass(pname: String) {
  var name = pname
  var devType = "PC"
  var port: PortClass = null //this is done to restrict each PC to one port
  var portReceived = -1
  var currentWeight = 0
  var ARPTable = new mutable.HashMap[String, String]()//stores map of IP address -> mac address (allows us to need fewer arp requests as the simulation progresses)
  
  var defaultGateway: String = "null" //IP address of this network's router
  var subnetMask: String = "null" //subnet mask helps us determine if another PC is within the same LAn as this PC
  var ipAddress: String  = "null"
  
  var storage = new mutable.HashMap[String, String]()
  
  def addPort(newPort: PortClass) {
    port = newPort //assign the input Port object to this PC's port varible.
  }
  
  
  //method for updateing/learning new mac addr/port combinations <-----run everytime the switch 
  def update(inputIP: String, inputMAC: String){
    
    if(!ARPTable.contains(inputMAC) ) {
      ARPTable += (inputIP -> inputMAC)//add the new entry to the table
    }
  }  
  
  //manually assign PC's subnetmask and defaultGateway
  def assignIP(newIP: String){
    if( !ScalaNetworkSimulator.globalIPaddressCheck.contains(newIP))
      ScalaNetworkSimulator.globalIPaddressCheck.+=(newIP)
      ipAddress = newIP //add this new ip to the globalIP address check table
      
     
  }
  
  def assignStorage( inputKey: String, inputValue: String){
    storage += ( inputKey -> inputValue )
  }
  
  def retrieveStorage( inputKey: String) = {
    storage.get(inputKey)
  }
  
  
  def checkIncomingDestMac(incomingPDU: PDU){
   
    if(incomingPDU.packet(3) == port.MACAddr){//determines if this PDU was sent for this mac address
      
      //now determine the action to take based on the pdu.packet(4)
      if( incomingPDU.packet(4) == "storeThis" ){//store what ever was sent to this device in storage hashMap
        assignStorage( incomingPDU.packet(5).toString() , incomingPDU.packet(8).toString() )
        //sendPDU with these values:
        /*
         *myPDU.packet(4) = "Acknowledged - successful"
         *myPDU.packet(5) = "null" 
         *myPDU.packet(6) =  "null"
         */
        println("Acknowledged - successful storage of file: " + incomingPDU.packet(5))
        
      }
      else if(incomingPDU.packet(4) == "replyWith"){//retrieve the Value of the sent Key and then send the value back to the sending PC
        retrieveStorage( incomingPDU.packet(5).toString() )
        //sendPDU with these values:
        /*
         *myPDU.packet(4) = "Acknowledged - successful"
         *myPDU.packet(5) = "null" 
         *myPDU.packet(6) =  "null"
         */
        println("Acknowledged - successful retrieval of data: " + incomingPDU.packet(6))
      }
      else if(incomingPDU.packet(4) == "ARPrequest"){
        //sendPDU()
        //sendPDU with these values:
        /*
         *myPDU.packet(4) = "ARPreply" -----> this will cause the calling device to record the source MAC address
         *myPDU.packet(5) = "source IP address" 
         *myPDU.packet(6) =  "source MAC address"
         */
      }
      else if(incomingPDU.packet(4) == "ping"){
        //print out the cumulative time value *2 which is stored in the incomingPDU.packet(6) = data String Value <-----*2 because this is the round trip time from the source IP to this destination IP
        println("from (" + ipAddress + ") time= " + incomingPDU.packet(6).toString().toInt*2 +  "ms" )
      }
      else if(incomingPDU.packet(4) == "traceroute"){
        //print out the cumulative hop count + 1 <-----stored in myPDU.packet(5) for ping cmd
        print( incomingPDU.packet(4) + " ")
        
        //print out the device name then then this port's ip address
        print( incomingPDU.packet(8).asInstanceOf[PCClass].name + " (" + incomingPDU.packet(8).toString() + ") " )
      
        //print out the cumulative time value *2 which is stored in the incomingPDU.packet(6) = data String Value <-----*2 because this is the round trip time from the source IP to this destination IP
        println(" time= " + incomingPDU.packet(6).toString().toInt*2 +  "ms")
      }
        
    }
    
    
  }
  
  
  
}//end of PCClass



class LinkClass {
  var deviceA: AnyRef = null
  var portA: PortClass = null
  var deviceB: AnyRef = null
  var portB: PortClass = null
}



class PortTypeClass(pname: String, pbandwith: Int, pspeed: Int) {
  var name = pname
  var bandwith = pbandwith
  var speed = pspeed
}



class PortClass(portNum: Int) {
  
  //Look in our PortType Global List for portType = object.name
  var portType: PortTypeClass = null
  var num = portNum
  var MACAddr: MACAddress = null
  var IPAddr: String = "None"
  var device: AnyRef = null
}


//------------------------------------------------------------------
class IPAddress(inputIP: String){
  
  //we are expecting inputIP like "192.168.12.19"
  
  var ipAddressValue: String = ""
  
  
  def sameSubnetTest(inputIPA: IPAddress, inputIPB: IPAddress, inputSubNetMask: IPAddress): Boolean = {
    
    //var tempBits: Array[Int] = new Array[Int](8) 
    var splitA: Array[String] = inputIPA.ipAddressValue.split(".")
    var splitB: Array[String] = inputIPB.ipAddressValue.split(".")
    var splitMask: Array[String] = inputSubNetMask.ipAddressValue.split(".")
    
    var bitsA: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer[Int]()
    var bitsB: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer[Int]()
    var bitsMask: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer[Int]()
    var temp: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer[Int]()
    
    var ansA: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer[Int]()
    var ansB: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer[Int]()
    
    
    var count: Int = 0
    
    //everything for bitsA
    for(i <- 0 until 3){
      temp = toBitArray( splitA(i) )
      
      for(j <- 0 until 7){
        bitsA(j + count) += temp(j)
      }
      
      count += 8
    }
    
   
    //everything for bitsB
    for(i <- 0 until 3){
      temp = toBitArray( splitB(i) )
      
      for(j <- 0 until 7){
        bitsB(j + count) += temp(j)
      }
      
      count += 8
    }
    
    //everything for bitsMask
    for(i <- 0 until 3){
      temp = toBitArray( splitMask(i) )
      
      for(j <- 0 until 7){
        bitsMask(j + count) += temp(j)
      }
      
      count += 8
    }

    
    for( index <- 0 until 31){
      if(bitsA(index) == 1 && bitsMask(index) == 1){
        ansA(index) = 1
      }
      else{
        ansA(index) = 0
      }
      
      if(bitsB(index) == 1 && bitsMask(index) == 1){
        ansB(index) = 1
      }
      else{
        ansB(index) = 0
      }
    }
    
    var sameSubnet: Boolean = true
    
    for( index <- 0 until 31){
      if(ansA(index) != ansB(index) ){
        sameSubnet = false
      }
      
      
    }
    
    
    if( sameSubnet == true )
      return true
    else
      return false
    
   
  }//end of sameSubnetTest
  
  
  
  def toBitArray(inputString: String): mutable.ArrayBuffer[Int] = {
    
    var bitArray: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer[Int]()
    var num: Int = inputString.toInt
    
    if( (num -128) >= 0 ){ //128
      bitArray.+=(1)
    }
    else{
      bitArray.+=(0)
    }
    
    if( (num - 64) >= 0 ){ //64
      bitArray.+=(1)
    }
    else{
      bitArray.+=(0)
    }
    
    if( (num - 32) >= 0 ){ //32
       bitArray.+=(1)
    }
    else{
      bitArray.+=(0)
    }
    
    if( (num - 16) >= 0 ){ //16
      bitArray.+=(1)
    }
    else{
      bitArray.+=(0)
    }
    
    if( (num -8) >= 0 ){ //8
      bitArray.+=(1)
    }
    else{
      bitArray.+=(0)
    }
    
    if( (num - 4) >= 0 ){ //4
       bitArray.+=(1)
    }
    else{
      bitArray.+=(0)
    }
    
    if( (num - 2) >= 0 ){ //2
      bitArray.+=(1)
    }
    else{
      bitArray.+=(0)
    }
    
    if( (num - 1) >= 0 ){ //1
      bitArray.+=(1)
    }
    else{
      bitArray.+=(0)
    }
    
    return bitArray
    
  }
  
  
  
  
}//end of IPAddress class





class MACAddress{
  
  var macAddressValue: String = ""
  
  def makeMAC(value: String) {
    macAddressValue = value.hashCode().toString
    ScalaNetworkSimulator.globalMACaddressCheck += (macAddressValue -> this)
  }
  

  def generateNewMAC{//generates a new unique mac address
    var newMac:String = "CCCCCC"
    
    
    var xyz  = scala.util.Random
    var temp: String = newMac
    var isUniqueMAC: Boolean = false
    
    
    do{
    for( counter <- 1 until 6){
           xyz.nextInt(16)
             
           if( xyz == 0 ){
           newMac.concat("0");
           } else if( xyz == 1 ){
           newMac.concat("1");
           } else if( xyz == 2 ){
           newMac.concat("2");
           } else if( xyz == 3 ){
           newMac.concat("3");
           } else if( xyz == 4 ){
           newMac.concat("4");
           } else if( xyz == 5 ){
           newMac.concat("5");
           } else if( xyz == 6 ){
           newMac.concat("6");
           } else if( xyz == 7 ){
           newMac.concat("7");
           } else if( xyz == 8 ){
           newMac.concat("8");
           } else if( xyz == 9 ){
           newMac.concat("9");
           } else if( xyz == 10 ){
           newMac.concat("A");
           } else if( xyz == 11 ){
           newMac.concat("B");
           } else if( xyz == 12 ){
           newMac.concat("C");
           } else if( xyz == 13 ){
           newMac.concat("D");
           } else if( xyz == 14 ){
           newMac.concat("E");
           } else{
           newMac.concat("F");
           }//end of if statemanets  
             
        }//end of loop
    
        
    if( !ScalaNetworkSimulator.globalMACaddressCheck.contains(newMac) ){//need to fix this object call
      isUniqueMAC = true
      ScalaNetworkSimulator.globalMACaddressCheck += (newMac -> this)
    }
    else{
      newMac = "CCCCCC"
    }
    } while(isUniqueMAC == false);
    
  }//end of function
  
 
  
}//end of MACAddress object








class PDU{//this class just gives us a structure to store the Data and the header information needed to send it accross networks
  
  var packet:Array[AnyRef] = new Array[AnyRef](9)
  
  
  
  /*  
   * myPDU.packet(0) = source IP address
   * myPDU.packet(1) = destination IP address
   * myPDU.packet(2) = source MAC address
   * myPDU.packet(3) = destination MAC address
   * 
   * myPDU.packet(4) = data instruction keyword; ex "storgeThis", "replyWith", "ARPrequest", to be sent (for ping set data to "ping", for traceroute set data to "traceroute")
   * myPDU.packet(5) = data String Key
   * myPDU.packet(6) = data String Value
   * 
   * myPDU.packet(7) = current port
   * myPDU.packet(8) = current device reference
   */
 
}//end of PDU object

