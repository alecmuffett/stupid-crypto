package Stupid;

/** a mutable holder for immutable values. No boiler plate getters and setters
    because this class is so simple. 

    Other alternatives to consider are java.lang.Atomic* boxed numeric types.
*/

public class Mutable<T> {
  public T value = null;
}
