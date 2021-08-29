package com.eastx.sap.batch.batchTdx;

import lombok.Data;

/**
 * The trade Raw-Object
 */
@Data
public class TradeBarRO {
	int datetime;
	int open;
	int high;
	int low;
	int close;
	int turnover;
	int volume;
	int filler;
}
