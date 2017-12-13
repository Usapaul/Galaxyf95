# Для работы с изображением нужно вызвать makefile с изменением переменной image 
# Например, >$ make image=f814  (без указания расширения, т.к. по умолчанию .fits)
image=Noimage
run=sex
comp=gfortran -fbounds-check
default=default
alldefault=$(default).sex $(default).param $(default).nnw $(default).conv $(default).psf
reg=regions.reg
ds9reg=-region $(reg)
# Если не нужны "red regions", то можно переменной rreg присвоить пустое значение (?)
redreg=regelell.reg
rreg=-region $(redreg)
invert=-cmap invert yes
scalaper=-scale log -scale limits -0.002 2.0
scalinit=-scale linear -scale limits -0.005 0.02
panreg=-region savedgal.reg
#--------------------------------------

# Создается файл с каталогом ok-объектов (и не только ok, но не no точно:))
$(image)ok.dat : foundlist.dat
	mv $< $@

foundlist.dat : saving comments.dat ellsorted.dat
	./saving
#	rm -f comments.dat
#	rm -f ellsorted.dat
#	rm -f onlyxy.dat $(reg) $(rreg)

# Программа совмещает список с данными и комментарии, полученные по каждому объекту
saving : saving.f95
	$(comp) $^ -o $@

comments.dat : panning.sh 
# Скрипт двигает курсор в ds9 и записывает комментарии
panning.sh : onlyxy.dat ds9opened
	./panning.sh

# Эти файлы появляются при выполнении reading
onlyxy.dat ellsorted.dat : $(reg)

# Вызов ds9 с удобными параметрами цветовой шкалы 
ds9opened : $(image).fits $(image)aper.fits $(reg)
	ds9 $(image)aper.fits $(ds9reg) $(rreg) $(scalaper) \
	$(image).fits $(ds9reg) $(rreg) $(invert) $(scalinit) &

# Для ds9 будут созданы файлы .reg с координатами для regions 
$(reg) : reading catalog.cat
	./reading

reading : reading.f95
	$(comp) $^ -o $@

# Программа, которая обрабатывает список с данными, получает на вход файл catalog.cat
catalog.cat : $(image)field.cat
	cp $< $@

# Sextractor работает с изображением
$(image)field.cat $(image)aper.fits : $(image).fits $(alldefault)
	$(run) $(image).fits \
	-CHECKIMAGE_TYPE APERTURES \
	-CHECKIMAGE_NAME $(image)aper.fits \
	-CATALOG_NAME $(image)field.cat


# Если хочу посмотреть, что я там сохранил, тем самым убрать лишнее
looksaved : forpansaved
	cp $(image)ok.dat checked.dat
	./forpansaved
	rm -f checked.dat
	ds9 $(image)aper.fits $(panreg) $(scalaper) \
	$(image).fits $(panreg) $(invert) $(scalinit) &	
	./panning.sh
	./saving
	rm -f savedgal.reg onlyxy.dat
	rm -f comments.dat ellsorted.dat
	mv foundlist.dat $(image)oknew.dat
forpansaved : forpansaved.f95
	$(comp) $^ -o $@