#!/usr/bin/env python2.5

import numpy as np

pi = 3.14159265
arcrad = 10
labrad = 17

r   = 80.         # Radius of sphere
R   = 0.65        # Fraction of r for inner radius of shell
ri  = r*R         # Inner radius of shell


x,y = 65.,30.     # This is where the photon will scatter
x1,y1 = x,y       
x2,y2 = -105.,60. # This is where the photon will end up
dx,dy = x2-x,y2-y

rnow = np.sqrt(x1*x1 + y1*y1)
rnorm = np.sqrt(dx*dx + dy*dy)

gamma = pi - np.arccos( (x*dx + y*dy)/(rnow*rnorm) )
i     = rnow*np.sin(gamma)

beta  = np.arcsin( np.sin(gamma)*rnow/ri )

# We want the solution where beta > pi/2 in order to get the
# correct distances travelled within the shell.
#
if ( beta <= 0.5*pi ) : beta = pi - beta
alpha = pi - beta - gamma

theta = np.arcsin( np.sin(gamma)*rnow/r )
if ( theta >= 0.5*pi ) : theta = pi - theta
xi    = pi - gamma - theta

p     = np.sqrt(ri*ri + rnow*rnow - 2.0*ri*rnow*np.cos(alpha))
q     = np.sqrt(ri*ri - i*i)

f = open('sphere.tex','w')

f.write('\psset{unit=.7mm}'+"\n")
f.write('\pspicture(-120,-30)(100,90)'+"\n")
# f.write("\\psframe(-120,-30)(100,90)\n")

# The shell
#
f.write('\\psarc[fillstyle=solid,fillcolor=lightgray]{-}'+ \
        "{%.1f}{%.1f}{%.1f}\n" %(r,10.,190.))
f.write('\\psarc[fillstyle=solid,fillcolor=white]{-}'+ \
        "{%.1f}{%.1f}{%.1f}\n" %(ri,9.,191.))

# Position and displacement vectors
#
f.write('\\psline[linestyle=dashed]{->}(0.,0.)(%.2f,%.2f)\n'%(x1,y1))
f.write('\psline[linewidth=1.0pt,linecolor=black]{->}'+ \
        "(%.2f,%.2f)(%.2f,%.2f)\n" %(x1,y1,x2,y2))
f.write('\pcline[linestyle=none]'+"(0.0,0.0)(%.1f,%.1f)\n"%(x1,y1))
f.write('\\bput{:U}{$\\posvec = (x,y,z)$}'+"\n")
f.write('\pcline[linestyle=none]'+"(%.1f,%.1f)(%.1f,%.1f)\n"%(x2,y2,x1,y1))
f.write('\\aput{:U}{$\\disvec$}'+"\n")
# f.write("\\rput[l](%.1f,%.1f){$(x,y,z)$}\n" %(x1+3,y1))
# f.write("\\rput[c](%.1f,%.1f){$(x+dx,y+dy,z+dz)$}\n" %(x2,y2+4))

foo = np.arctan2(y2-y1,x2-x1)*180./pi
f.write('\\rput[c]'+"(%.1f,%.1f){" %(x1,y1))
f.write('  \SpecialCoor\psarc{-}'+"{%d}{%.3f}{%.3f}\n" \
        %(arcrad,foo,foo+gamma*180.0/pi))
f.write('  \\rput[c]'+"(%.3f;%.3f){$\\gamma$}}\n" \
        %(14,foo+gamma*90./pi))

# The distance p
#
p1 = p*np.cos(foo*pi/180.) + x
p2 = p*np.sin(foo*pi/180.) + y
#f.write('\pcline[linestyle=none]'+"(%.2f,%.2f)(%.2f,%.2f)\n"%(p1,p2,x,y))
#f.write('\\aput{:U}{$p$}\n')

# The inner radius
#
f.write('\\psline[linestyle=dashed]'+"(%.2f,%.2f)(%.2f,%.2f)\n"%(0.,0.,p1,p2))
f.write('\\pcline[linestyle=none]'+"(%.2f,%.2f)(%.2f,%.2f)\n"%(0.,0.,p1,p2))
f.write('\\aput{:U}{$r_i$}\n')


# The angle beta
#
#f.write('\\rput[c]'+"(%.3f,%.3f){" %(p1,p2))
#f.write('  \SpecialCoor\psarc{-}'+"{%d}{%.3f}{%.3f}\n" \
#        %(5.,foo+(pi-beta)*180./pi,foo+180))
#f.write('  \\rput[c]'+"(%.3f;%.3f){$\\beta$}}\n"
#        %(9.,foo+180-beta*90./pi))


# The angle alpha
#
#tmp1 = np.arctan2(y,x)
#tmp2 = np.arctan2(p2,p1)
#f.write('\\psarc{-}'+"{%d}{%.2f}{%.2f}\n"%(16,tmp1*180./pi,tmp2*180./pi))
#f.write("\\rput[c](0.,0.){\SpecialCoor\n")
#f.write("  \\rput[c](%.2f;%.2f){$\\alpha$}}\n" %(22,0.5*(tmp1+tmp2)*180./pi))

q1 = p1 + q*np.cos(foo*pi/180.)
q2 = p2 + q*np.sin(foo*pi/180.)
f.write('\pcline[linestyle=none]'+"(%.2f,%.2f)(%.2f,%.2f)\n"%(q1,q2,p1,p2))
f.write('\\aput{:U}{$q$}\n')
f.write('\\psline[linestyle=dashed](0.,0.)'+"(%.2f,%.2f)\n" %(q1,q2))
f.write('\\pcline[linestyle=none](0.,0.)'+"(%.2f,%.2f)\n" % (q1,q2))
f.write('\\aput{:U}{$i$}\n')
f.write('\\rput[c]'+"{%.3f}(%.2f,%.2f){" %(foo,q1,q2))
f.write(  '\\psline'+"(%.2f,%.2f)(%.2f,%.2f)(%.2f,%.2f)}\n" %(0.,4,-4,4,-4,0))

rpath = r*(np.cos(theta) + np.sqrt((rnow/r)**2 - np.sin(theta)**2))
#rpath = np.sqrt(rnow*rnow + r*r - 2.0*rnow*r*np.cos(xi));

s1 = rpath*np.cos(foo*pi/180.) + x
s2 = rpath*np.sin(foo*pi/180.) + y
f.write('\\rput[c]'+"(%.3f,%.3f){" %(s1,s2))
f.write('  \SpecialCoor\psarc{-}'+"{%d}{%.3f}{%.3f}\n" \
        %(15.,foo+(pi-theta)*180./pi,foo+180))
f.write('  \\rput[c]'+"(%.3f;%.3f){$\\theta$}}\n"
        %(18.,foo+180-theta*90./pi))


# The angle xi
#
#bar = gamma*180./pi-(180.-foo)
#f.write('\\psarc{-}'+"{%d}{%.3f}{%.3f}\n" \
#        %(7.,bar,bar+xi*180./pi))
#f.write('\\rput*[c]'+"(%.3f,%.3f){$\\xi$}\n"%(1,11))

f.write("\\rput[c]{%.2f}(0.,0.){\n"%(foo-theta*90./pi))
f.write("  \\SpecialCoor\n")
f.write("  \\psline[linestyle=dashed](0.;0.)(%.2f;%.3f)\n"%(r,-theta*90/pi))
f.write('  \\pcline[linestyle=none]'+"(%.2f;%.2f)(0.;0.)\n" \
        %(r,-theta*90./pi))
f.write('  \\aput{:U}{$r$}\n')
f.write("  \\psline[linestyle=dashed](0.;0.)(%.2f;%.3f)\n" \
        %(r,theta*90/pi))
f.write("  \\psarc{-}{%d}{%.2f}{%.2f}\n" %(30,-theta*90./pi,0.0))
f.write("  \\rput[c]{180}(%d;%.3f){$\\frac{\\theta}{2}$}\n" %(35,-0.5*theta*90./pi))
tmp = r*np.cos(0.5*theta)
f.write("  \\psline[linestyle=dashed](0.,0.)(%.2f;%.3f)\n" \
        %(tmp,0.))
f.write("  \\psline[linestyle=dashed](%.2f;%.2f)(%.2f;%.2f)\n" \
        %(r,-theta*90./pi,r,theta*90./pi))
f.write("  \\pcline[linestyle=none](%.2f;%.2f)(%.2f;%.3f)\n" \
        %(r,theta*90./pi,r,-theta*90./pi))
f.write("  \\bput*{:R}{$l$}\n")


# The plane P
#
f.write(" \\rput{%.2f}(%.2f;%.2f){\n" %(pi-foo,r,theta*90./pi))
f.write("  \\SpecialCoor\n")
f.write("  \\psline[linewidth=2pt,linestyle=solid]{-}(30;-90)(65.;90.)\n")
f.write("  \\pcline[linestyle=none](30;-90)(65.;90.)\n")
f.write("  \\aput{:R}{$P$}}\n")


phi = 0.5*(pi-theta)
psi = pi-theta-phi

f.write(" \\rput{90}(%.2f;%.2f){\n" %(r,-theta*90./pi))
#f.write("  \\psline[linecolor=red]{->}(0,0)(0.,10.)\n")
f.write("   \\psarc{-}{%d}{%.2f}{%.2f}\n" %(7,-phi*180./pi,phi*180./pi))
f.write("   \\psline(%.2f;%.2f)(%.2f;%.2f)\n" \
        %(6,-0.5*phi*180./pi,8,-0.5*phi*180./pi))
f.write("   \\psline(%.2f;%.2f)(%.2f;%.2f)\n" \
        %(6,0.5*phi*180./pi,8,0.5*phi*180./pi))
f.write("   \\rput{%.2f}(%.2f;%.2f){$\\varphi$}}}\n" \
        %(theta*90./pi-foo-90,12,0.5*phi*180./pi))

#f.write("  \\rput(%.2f;%.2f){\\psarc{-}{%d}{%.2f}{%.2f}\n" \
#        %(r,-theta*90./pi,7,90-phi*180./pi,180-theta*90./pi))
#f.write("    \\rput{225}(%.2f;%.2f){$\\varphi$}\n" %(10,90+1.5*theta*180./pi))
#f.write("    \\psline(%.2f;%.2f)(%.2f;%.2f)\n" %(6,135,8,135))
#tt = 90.-.5*phi*180./pi
# f.write("    \\psline(%.2f;%.2f)(%.2f;%.2f)\n" %(6,tt,8,tt))
#f.write("    \\rput{225}(%.2f;%.2f){$\\varphi$}}}\n" %(10,tt))


# The plane perpendicular to the line of sight
#
xx = 2*r*np.sin(0.5*theta)
d = 2*r*(np.sin(0.5*theta))**2
tmp = xx*np.sin(phi)
w1 = d*np.cos(foo*pi/180.) + s1
w2 = d*np.sin(foo*pi/180.) + s2


f.write("\\pcline[linestyle=none](%.2f,%.2f)(%.2f,%.2f)\n"%(w1,w2,s1,s2))
f.write("\\aput{:U}{$d$}\n")




f.write('\endpspicture'+"\n")
f.close()
