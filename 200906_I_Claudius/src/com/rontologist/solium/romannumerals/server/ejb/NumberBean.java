package com.rontologist.solium.romannumerals.server.ejb;

import com.rontologist.solium.romannumerals.common.ejb.NumberRemote;
import javax.ejb.*;

@Stateless
public class NumberBean implements NumberRemote {

    public String echo(String s) {
        return s;
    }

}