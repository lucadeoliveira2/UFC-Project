# JIRI VS ALEX #
extra.function('Jiri Prochazka', 'Alex Pereira', 'TRUE', 'M', 'Light Heavyweight') # All got Jiri

# PAVLOVICH VS ASPINALL #
extra.function('Sergei Pavlovich', 'Tom Aspinall', 'TRUE', 'M', 'Heavyweight')# All got Sergei

# ANDRADE VS DERN #
extra.function('Jessica Andrade', 'Mackenzie Dern', 'FALSE', 'F', "Women's Strawweight") # SVM got Dern

# FREVOLA VS BSD #
extra.function('Matt Frevola', 'Benoit Saint Denis', 'FALSE', 'M', 'Lightweight') # All got BSD

# LOPES VS SABATINI #
extra.function('Diego Lopes', 'Pat Sabatini', 'FALSE', 'M', 'Featherweight') # LOG and NN got S

# ERCEG VS COSTA #
extra.function('Steve Erceg', 'Alessandro Costa', 'FALSE', 'M', 'Flyweight') # SVM got E

# RICCI VS GODINEZ #
extra.function('Tabatha Ricci', 'Loopy Godinez', 'FALSE', 'F', "Women's Strawweight") # NN got Ricci

# REBECKI VS ROBERTS
extra.function('Mateusz Rebecki', 'Roosevelt Roberts', 'FALSE', 'M', 'Lightweight') # all got R

# SADYKHOV VS BORSHCHEV #
extra.function('Nazim Sadykhov', 'Viacheslav Borshchev', 'FALSE', 'M', 'Lightweight') # SVM, LOG and NN got B, Forest got S

# GORDON VS MADSEN #
extra.function('Jared Gordon', 'Mark Madsen', 'FALSE', 'M', 'Lightweight') #SVM got Madsen, Forest got Gordon

# CASTANEDA VS KANG #
extra.function('John Castaneda', 'Kyung Ho Kang', 'FALSE', 'M', 'Bantamweight') # All but Forest got Castaneda


subset(full_df, fighter_fullname == 'Nick Fiore')
