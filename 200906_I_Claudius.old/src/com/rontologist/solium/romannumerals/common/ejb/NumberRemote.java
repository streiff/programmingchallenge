package com.rontologist.solium.romannumerals.common.ejb;

import com.rontologist.solium.romannumerals.server.number.RomanNumber;

import javax.ejb.Local;
import javax.ejb.Remote;
import java.io.Serializable;

@Remote
public interface NumberRemote extends Serializable {
    RomanNumber getRomanNumber(String s);
}
