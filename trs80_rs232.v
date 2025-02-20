module trs80_rs232 (
	input         clk,
	input         reset,
	input   [7:0] cfg,
	input   [2:0] addr,
	input         e8,
	input         in,
	input         out,
	input   [7:0] din,
	output reg [7:0] dout,
	output        oe,

	input         uart_rx,
	input         uart_cts,
	output        uart_tx,
	output reg    uart_rts
);

assign     oe = e8 & !addr[2] & in;
wire [7:0] uart_dout;
reg        uart_reset, send_break;
wire       thrl, pe, fe, ove, tre, thre, dr, tx;
assign     uart_tx = send_break ? 1'b1 : tx;
reg  [3:0] tx_baud, rx_baud;
wire       rx_clk_en, tx_clk_en;

always @(*) begin
	case (addr[1:0])
		0: dout = {uart_cts, 1'b0 /*dsr*/, 1'b0 /*cd*/, 1'b0 /*ri*/, 3'b111, uart_rx};
		1: dout = ~cfg;
		2: dout = {dr, thre, ove, fe, pe, 3'b111};
		3: dout = uart_dout;
		default: ;
	endcase
end

always @(posedge clk) begin
	uart_reset <= 0;
	if (e8 & !addr[2] & out)
		case (addr[1:0])
			0: uart_reset <= 1;
			1: {tx_baud, rx_baud} <= din;
			2: {send_break, uart_rts} <= {~din[2], din[1]};
			default: ;
		endcase
end

trs80_rs232_clockdiv rx_clock(clk, reset, rx_baud, rx_clk_en);
trs80_rs232_clockdiv tx_clock(clk, reset, tx_baud, tx_clk_en);

gen_uart_tr_1602 tr_1602(
	.clk(clk),
	.reset(uart_reset),
	.rx_clk_en(rx_clk_en), // bitrate x 16
	.tx_clk_en(tx_clk_en), // bitrate x 16
	.din(din),
	.dout(uart_dout),
	.thrl(e8 & addr == 3'b011 & out),  // transmitter holding register load
	// status
	.pe(pe),      // parity error
	.fe(fe),      // framing error
	.oe(ove),     // overrun error
	.thre(thre),  // transmitter holding register empty
	.tre(tre),    // transmitter register empty
	.dr(dr),      // data received
	.drr_n(~(e8 & addr == 3'b011 & in)), // data received reset
	// control
	.crl(e8 & addr == 3'b010 & out), // control register load
	.pi(din[3]),  // parity inhibit
	.sbs(din[4]), // stop bit select (not implemented)
	.wls({din[5], din[6]}), // word length select (5-6-7-8 bits)
	.epe(din[7]), // even parity enable
	// uart pins
	.rx(uart_rx),
	.tx(tx)
);

endmodule

module trs80_rs232_clockdiv (
	input clk, // 42 MHz clock assumed
	input reset,
	input [3:0] div,
	output reg div_en
);

reg  [15:0] cnt;
reg  [15:0] val;

always @(*) begin
	case (div)
		0: val=52499; //50 baud
		1: val=34999; //75 baud
		2: val=23863; //110 baud
		3: val=19516; //134.5 baud
		4: val=17499; //150 baud
		5: val=8749; //300 baud
		6: val=4374; //600 baud
		7: val=2187; //1200 baud
		8: val=1457; //1800 baud
		9: val=1312; //2000 baud
		10: val=1093; //2400 baud
		11: val=728; //3600 baud
		12: val=546; //4800 baud
		13: val=364; //7200 baud
		14: val=272; //9600 baud
		15: val=136; //19200 baud
		default: ;
	endcase
end

always @(posedge clk) begin
	div_en <= 0;
	if (reset)
		cnt <= 0;
	else begin
		cnt <= cnt + 1'd1;
		if (cnt == val) begin
			cnt <= 0;
			div_en <= 1;
		end
	end
end

endmodule
