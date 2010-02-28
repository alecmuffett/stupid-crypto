// java ROT13 client

import Stupid.Mutable;

public class Rot13cli {

  public static void main(String[] args) throws java.io.IOException {
    System.out.println("ROT13 Stupid-Java commandline tool");
    int c;
    while( (c = System.in.read()) != -1 ) {
        System.out.write(rot13char(c));
    }
    System.out.flush();
  }


  public static int rot13char(int c) {

    Mutable<Short> inputWrapper = new Mutable<Short>();
    inputWrapper.value = (short)c;

    Mutable<Short> outputWrapper = new Mutable<Short>();

    rot13.rot13(outputWrapper, inputWrapper);

    return outputWrapper.value;
  }

/* the signature for rot13 looks like this:
import Stupid.Mutable;
class StupidGenerated {
public void rot13(Mutable<Short> output,Mutable<Short> input) {

*/

}

