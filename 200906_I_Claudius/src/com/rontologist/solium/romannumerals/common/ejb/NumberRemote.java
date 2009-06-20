package com.rontologist.solium.romannumerals.common.ejb;
import javax.ejb.*;

@Remote
public interface NumberRemote {
    String echo(String s);
}
