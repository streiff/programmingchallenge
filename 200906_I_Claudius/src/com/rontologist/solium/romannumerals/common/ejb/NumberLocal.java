package com.rontologist.solium.romannumerals.common.ejb;

import com.rontologist.solium.romannumerals.server.number.RomanNumber;

import javax.ejb.Remote;

@Remote
public interface NumberRemote {
    RomanNumber getRomanNumber(String s);
}
