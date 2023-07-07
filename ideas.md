# Specs

## Conv core

### Max kernel size: 3x3

Editable

### Max input size: 22x22

22x22=484 < 512  
23x23=529 > 512

With 3x3 kernel and padding, this core can handle an 20x20 input at once, or 40x40

### Process

If padding is enabled, then a reset of line buffer can be used to skip the first line and the first pixel on the second line

# Components

## 

# Flows(obsoleted)

## Load param

1. Loader enter **ST_IDLE** state, initialize

2. Software set:
    - `Param_Addr`
    - `Param_Len`
    - `Target_Core`

3. Software set flag `Load_Start` to start load
   - Hardware set `DDR_Addr`, `DDR_LineLine`, `DDR_LineCount=1`, `DDR_LineOffset=1`

4. Loader enters **ST_WAIT** state, waiting for target core to be idle
   - Waiting for core's **IDLE** signal

6. Loader enters **ST_START** state, sending data on bus _AR_
   - Hardware selects target core
   - Target core enters **ST_PARAM** state

7. Loader enters **ST_LOAD** state, loading data on bus _R_, sending to target bus

8. When load finishes, loader enters **ST_IDLE** state

## Load data

1. Software set:
   - `Input_Addr`
   - `Input_LineLen`
   - `Input_LineOffset`
   - `Input_LineCount`
   - `Target_Core`
   - `Op`

2. Hardware calculate these based on `Target_Core` and `Op`:
   - `BRAM_Addr`
   - `BRAM_LineLen`
   - `BRAM_LineCount`
