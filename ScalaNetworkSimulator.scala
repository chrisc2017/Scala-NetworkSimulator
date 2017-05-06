package ScalaNetworkSimulator

object ScalaNetworkSimulator  extends NetworkSimulator{
  
  def main( args: Array[String] ){

    RoutingProtocol name "rp1";
      learn by "speed";
      choose port "speed";
    
    Switch name "sw1";
      port num 3 uses "Fiber";
      port num 4 uses "Fiber";
      port num 5 uses "Fiber";
    
    Switch name "sw2"
      port num 10 uses "Fiber";
      port num 11 uses "Fiber";
      port num 12 uses "Fiber";
      
    Router name "r1";
      port num 6 uses "Fiber" IPAddress "192.168.0.1";
      port num 7 uses "Fiber" IPAddress "10.10.10.1";
      
    Router name "r2";
      port num 8 uses "Fiber" IPAddress "10.10.10.2";
      port num 9 uses "Fiber" IPAddress "44.44.44.1";
      
    PC name "Bob's_PC";
      port num 1 uses "Fiber" IPAddress "192.168.0.36";
      default gateway "192.168.0.1";
    
    PC name "Joe's_PC";
      port num 2 uses "Fiber" IPAddress "192.168.0.21";
      default gateway "192.168.0.1";
      
    PC name "Smith's_PC";
      port num 13 uses "Fiber" IPAddress "44.44.44.8";
      default gateway "10.10.10.2";
    
    PC name "Guest's_PC";
      port num 14 uses "Fiber" IPAddress "44.44.44.3";
      default gateway "10.10.10.2";
      
    Links;
      connect deviceA "sw1" portA 3 deviceB "Bob's_PC" portB 1;
      connect deviceA "sw1" portA 4 deviceB "Joe's_PC" portB 2;
      connect deviceA "sw1" portA 5 deviceB "r1" portB 6;
      connect deviceA "r1" portA 7 deviceB "r2" portB 8;
      connect deviceA "r2" portA 9 deviceB "sw2" portB 10;
      connect deviceA "sw2" portA 11 deviceB "Smith's_PC" portB 13;
      connect deviceA "sw2" portA 12 deviceB "Guest's_PC" portB 14;
      
    Config Network;
    
    
  }
 
}






