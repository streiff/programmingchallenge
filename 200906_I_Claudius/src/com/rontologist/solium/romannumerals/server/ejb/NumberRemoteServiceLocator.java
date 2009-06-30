package com.rontologist.solium.romannumerals.server.ejb;

import com.rontologist.solium.romannumerals.common.ejb.NumberRemote;

import javax.naming.InitialContext;
import javax.naming.NamingException;

public class NumberRemoteServiceLocator {
    private static final NumberRemoteServiceLocator instance = new NumberRemoteServiceLocator();

    private NumberRemote numberRemote = null;

    private NumberRemoteServiceLocator() {}

    public static NumberRemoteServiceLocator getInstance() {
	return instance;
    }

    public NumberRemote getNumberRemote() {
	if (numberRemote == null) {
	    try {
		numberRemote = (NumberRemote) new InitialContext().lookup("ejb/NumberBean");
	    } catch (NamingException e) {
		numberRemote = null;
		throw new RuntimeException("Could not find number remote bean");
	    }
	}
	return numberRemote;
    }
}
