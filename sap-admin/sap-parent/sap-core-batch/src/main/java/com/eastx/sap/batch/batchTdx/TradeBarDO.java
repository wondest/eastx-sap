package com.eastx.sap.batch.batchTdx;

import lombok.Data;

/**
 * The trade Data-Object
 */
@Data
public class TradeBarDO {
	String datetime;
	Double open;
	Double close;
	Double high;
	Double low;
	Double turnover;
	Double volume;

	@Override
	public String toString() {
		return new StringBuffer().append(datetime).append(",").append(open).append(",").toString();
	}
}
