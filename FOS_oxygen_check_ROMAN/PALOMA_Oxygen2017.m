close all
clear
clc

oldpath=cd;

%[Station,Cond,Temp,Sal,Pres,Den,PotDen,SWTemp,DOml_l,DOumol_kg,a,DOumol_l,flag]...
%    =textscan('SBE37SMP-ODO-RS232_2017-3.asc','%s %{mm/dd/yyyy}D %{hh:mm:ss}T %f %f %f %f %f %f %f %f %f %f %f %u');

%% Load in situ data from text file
fid=fopen('Paloma March through Nov 2017 Oxygen2.txt');
a=textscan(fid,'%s %s %s %f %f %f %f %f %f %f %f %f %f %f %u');%, 'CollectOutput', true); %{mm/dd/yyyy}D
sts=fclose(fid);

c=datevec(char(a{3}), 'HH:MM:SS');
time=[c(:,4) c(:,5) c(:,6)]; %getting the proper time in HH MM SS
Decimal_time=time(:,1)/24+time(:,2)/60/24+time(:,3)/60/60/24;
Sal=a{6}; %Salinity, PSU
Temp=a{5}; %Temperature, C
Press=a{7}; %Pressure, dbar
date=a{2};
date2=datenum(date, 'mm/dd/yyyy');
date3=datetime(date2+Decimal_time,'ConvertFrom','datenum');

clear c
oxygen=a{14}; %oxygen data in umol/l

%% Load Winkler data from excel
[num, txt, raw]=xlsread('PALOMA DO winkler 2017.xlsx','A5:H36');
%Date_winkler=x2mdate(num(:,1)); %Convert excel date to matlab date
%Date_winkler2=x2mdate(num(:,2));
%time_winkler=datenum(num(:,3));
Date_winkler=datenum(char(txt(:,1)), 'dd.mm.yyyy');
Date_winkler2_1=datetime(num(:,1)+num(:,2),'ConvertFrom','excel');
Date_winkler2_2=datetime(num(:,1)+num(:,3),'ConvertFrom','excel');
Date_winkler2_3=[Date_winkler2_1 Date_winkler2_2];
Date_winkler2=datetime(num(:,1)+(num(:,2)+num(:,3))/2,'ConvertFrom','excel');
Date_winkler3_1=datenum(Date_winkler2_1);
Date_winkler3_2=datenum(Date_winkler2_2);
Date_winkler3=datenum(Date_winkler2);%,'ConvertFrom','juliandate');
oxygen_winkler=num(:,7); %Winkler oxygen data in umol/l

%% Plot data
figure
plot(date3,oxygen,'*')
grid on
hold on
plot(Date_winkler2,oxygen_winkler,'ro', 'MarkerFaceColor', 'r', 'MarkerEdgeColor', 'k')
hold off
xlim([min(date3)-10 max(date3)+10]);

%% Check for linear drift
drift_date=[];
drift=[];
index=find(Date_winkler2>min(date3) & Date_winkler2<max(date3));
for i=1:length(Date_winkler3(index));
    %x=abs(date3-Date_winkler2(index(i)));
    %y=min(abs(date3-Date_winkler2(index(i))));
    cp_check=find(date3>Date_winkler2_1(index(i)) & date3<Date_winkler2_2(index(i))); %find(x==y); %find point closest to Winkler titration
    drift_date=[drift_date; Date_winkler2(index(i)) mean(date3(cp_check)) Date_winkler2_1(index(i)) Date_winkler2_2(index(i))];
    drift=[drift; mean(oxygen(cp_check))-oxygen_winkler(index(i)) Date_winkler3(index(i))/10^5 mean(date2(cp_check))/10^5 length(cp_check) mean(Sal(cp_check)) mean(Temp(cp_check)) mean(Press(cp_check)) std(oxygen(cp_check)) std(Sal(cp_check)) std(Temp(cp_check)) std(Press(cp_check))];
end
drift_date2=[mean(drift_date([1,2,3])); mean(drift_date([4,5,6]));...
    mean(drift_date([7,8,9])); mean(drift_date([10,11,12]));...
    mean(drift_date([13,14,15])); mean(drift_date([16,17]))];
drift2=[mean(drift([1,2,3])); mean(drift([4,5,6]));...
    mean(drift([7,8,9])); mean(drift([10,11,12]));mean(drift([13,14,15])); mean(drift([16,17]))];

mdl=(drift(:,2)*10^5-min(drift(:,2))*10^5)\[ones(length(drift(:,1)),1) drift(:,1)];
mdl2=fitlm(drift(:,2)*10^5-min(drift(:,2))*10^5, drift(:,1))

x=[ones(size(drift(:,1))) drift(:,5) (drift(:,6)+273.15) drift(:,7)];
x2=[drift(:,5) (drift(:,6)+273.15) drift(:,7)];
multiple_LR=regress(drift(:,1),x)
multiple_LR2=fitlm(x2,drift(:,1),'linear')

figure
plot(drift_date(:,1), drift(:,1),'ro')
hold on
plot(drift_date2(:,1), drift2(:,1),'go-')
hold off

figure
plot(drift(:,2)*10^5-min(drift(:,2))*10^5, drift(:,1),'ro')
hold on
%plot(drift(:,2)*10^5-min(drift(:,2))*10^5, drift2(:,1),'go-')
%plot(drift(:,2)*10^5-min(drift(:,2))*10^5, mdl(2)*(drift(:,2)*10^5-min(drift(:,2))*10^5)+mdl(1), 'k-')
plot(drift(:,2)*10^5-min(drift(:,2))*10^5, -0.017372*(drift(:,2)*10^5-min(drift(:,2))*10^5)-2.9239, 'k-')
hold off

%%
%Try Multiple Linear regression (Sal, Temp, Press) correction with in situ oxygen data
figure
%subplot(3,1,1)
subplot(2,1,1)
plot(date3,oxygen,'g*')
hold on
grid on
plot(Date_winkler2,oxygen_winkler,'ro','MarkerFaceColor', 'r', 'MarkerEdgeColor', 'k')
hold off
set(gca,'fontsize',20)
ylabel('Oxygen Concentratin (\mumol/L)')
legend('In-situ Oxygen (\mumol/L)','Winkler Oxygen (\mumol/L)')
text(min(date3)-5,290,'a)','FontSize',22)
xlim([min(date3)-10 max(date3)+10]);

%subplot(3,1,2)
subplot(2,1,2)
plot(date3,oxygen,'g*')
hold on
grid on
plot(date3,oxygen-multiple_LR(1)-multiple_LR(2)*Sal-multiple_LR(3)*(Temp+273.15)-multiple_LR(4)*Press,'*')
plot(Date_winkler2,oxygen_winkler,'ro','MarkerFaceColor', 'r', 'MarkerEdgeColor', 'k')
hold off
set(gca,'fontsize',20)
ylabel('Oxygen Concentratin (\mumol/L)')
legend('In-situ Oxygen (\mumol/L)','MLR Corrected In-situ Oxygen (\mumol/L)','Winkler Oxygen (\mumol/L)')
text(min(date3)-5,290,'b)','FontSize',22)
xlim([min(date3)-10 max(date3)+10]);

%subplot(3,1,3)
%plot(date3,oxygen,'g*')
%hold on
%grid on
%plot(date3,oxygen+0.017372*(date2-min(date2))+2.9239,'*')
%plot(Date_winkler2,oxygen_winkler,'ro','MarkerFaceColor', 'r', 'MarkerEdgeColor', 'k')
%hold off
%set(gca,'fontsize',20)
%ylabel('Oxygen Concentratin (\mumol/L)')
%legend('In-situ Oxygen (\mumol/L)','LR Corrected In-situ Oxygen (\mumol/L)','Winkler Oxygen (\mumol/L)')
%xlim([min(date3)-10 max(date3)+10]);

%% Check for new offset
%MLR offset
MLR_oxygen=oxygen-(multiple_LR(1)+multiple_LR(2)*Sal+multiple_LR(3)*(Temp+273.15)+multiple_LR(4)*Press);
MLR_drift_date=[];
MLR_drift=[];
index=find(Date_winkler2>min(date3) & Date_winkler2<max(date3));
for i=1:length(Date_winkler3(index));
    %x=abs(date3-Date_winkler2(index(i)));
    %y=min(abs(date3-Date_winkler2(index(i))));
    %cp_check=find(x==y); %find point closest to Winkler titration
    cp_check=find(date3>Date_winkler2_1(index(i)) & date3<Date_winkler2_2(index(i)));
    MLR_drift_date=[MLR_drift_date; Date_winkler2(index(i)) mean(date3(cp_check))];
    MLR_drift=[MLR_drift; mean(MLR_oxygen(cp_check))-oxygen_winkler(index(i)) Date_winkler3(index(i))/10^5 mean(date2(cp_check))/10^5 length(cp_check) mean(Sal(cp_check)) mean(Temp(cp_check)) mean(Press(cp_check)) std(MLR_oxygen(cp_check))];
end
MLR_drift_date2=[mean(MLR_drift_date([1,2,3])); mean(MLR_drift_date([4,5,6]));...
    mean(MLR_drift_date([7,8,9])); mean(MLR_drift_date([10,11,12]));...
    mean(MLR_drift_date([13,14,15])); mean(MLR_drift_date([16,17]))];
MLR_drift2=[mean(MLR_drift([1,2,3])); mean(MLR_drift([4,5,6]));...
    mean(MLR_drift([7,8,9])); mean(MLR_drift([10,11,12]));mean(MLR_drift([13,14,15])); mean(MLR_drift([16,17]))];

%SLR offset
SLR_oxygen=oxygen-(-0.017372*(date2-min(date2))-2.9239);
SLR_drift_date=[];
SLR_drift=[];
index=find(Date_winkler2>min(date3) & Date_winkler2<max(date3));
for i=1:length(Date_winkler3(index));
    x=abs(date3-Date_winkler2(index(i)));
    y=min(abs(date3-Date_winkler2(index(i))));
    cp_check=find(x==y); %find point closest to Winkler titration
    SLR_drift_date=[SLR_drift_date; Date_winkler2(index(i)) date3(cp_check)];
    SLR_drift=[SLR_drift; SLR_oxygen(cp_check)-oxygen_winkler(index(i)) Date_winkler3(index(i))/10^5 date2(cp_check)/10^5 length(cp_check) Sal(cp_check) Temp(cp_check) Press(cp_check)];
end
SLR_drift_date2=[mean(SLR_drift_date([1,2,3])); mean(SLR_drift_date([4,5,6]));...
    mean(SLR_drift_date([7,8,9])); mean(SLR_drift_date([10,11,12]));...
    mean(SLR_drift_date([13,14,15])); mean(SLR_drift_date([16,17]))];
SLR_drift2=[mean(SLR_drift([1,2,3])); mean(SLR_drift([4,5,6]));...
    mean(SLR_drift([7,8,9])); mean(SLR_drift([10,11,12]));mean(SLR_drift([13,14,15])); mean(SLR_drift([16,17]))];

%Mean and stdev of offset, MLR correction, and SLR correction
Offset_mean=mean(drift(:,1));
Offset_stdev=std(drift(:,1)-Offset_mean);
MLR_mean=mean(MLR_drift(:,1));
MLR_stdev=std(MLR_drift(:,1)-MLR_mean);
SLR_mean=mean(SLR_drift(:,1));
SLR_stdev=std(SLR_drift(:,1)-SLR_mean);

figure
%subplot(3,1,1)
subplot(2,1,1)
plot(drift_date(:,1), drift(:,1),'ro')
hold on
grid on
plot(drift_date2(:,1), drift2(:,1),'go-')
hold off
set(gca,'fontsize',20)
%title('In situ-Winkler Residuals')
ylabel('Residual (\mumol/L)')
text_offset=['Mean=', num2str(Offset_mean), ' \mumol/L'];
text_offset2=['St Error=', num2str(Offset_stdev/sqrt(length(drift))), ' \mumol/L'];
text(drift_date(1),1.5,'a)','FontSize',22)
text(drift_date(13), max(drift(:,1))-0.5, text_offset,'FontSize',18)
text(drift_date(13), max(drift(:,1))-1, text_offset2,'FontSize',18)

%subplot(3,1,2)
subplot(2,1,2)
plot(MLR_drift_date(:,1), MLR_drift(:,1),'ro')
hold on
grid on
plot(MLR_drift_date2(:,1), MLR_drift2(:,1),'go-')
hold off
set(gca,'fontsize',20)
%title('MLR-Winkler Residuals')
ylabel('Residual (\mumol/L)')
%ylim([-5.5,5.5])
text_offset=['Mean=', num2str(MLR_mean), ' \mumol/L'];
text_offset2=['St Error=', num2str(MLR_stdev/sqrt(length(MLR_drift))), ' \mumol/L'];
text(drift_date(1),3.5,'b)','FontSize',22)
text(MLR_drift_date(13), max(MLR_drift(:,1)), text_offset,'FontSize',18)
text(MLR_drift_date(13), max(MLR_drift(:,1))-0.5, text_offset2,'FontSize',18)

%subplot(3,1,3)
%plot(SLR_drift_date(:,1), SLR_drift(:,1),'ro')
%hold on
%grid on
%plot(SLR_drift_date2(:,1), SLR_drift2(:,1),'go-')
%hold off
%title('SLR-Winkler Residuals')
%ylabel('Residual (\mumol/L)')
%text_offset=['Mean=', num2str(SLR_mean), ' \mumol/L'];
%text_offset2=['St Dev=', num2str(SLR_stdev/sqrt(length(SLR_drift))), ' \mumol/L'];
%text(SLR_drift_date(13), 7.25, text_offset,'FontSize',18)
%text(SLR_drift_date(13), 6.75, text_offset2,'FontSize',18)


cd(oldpath);