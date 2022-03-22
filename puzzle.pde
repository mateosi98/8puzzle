import de.bezier.guido.*;
import java.util.Random;
import java.util.List;
import java.util.function.Consumer;
import java.io.*;
import java.util.concurrent.Executors;
import java.util.Arrays;

int sz, n=4, nPrev = 3, pausa[] = {0,0,0,0},a=-1,b=-1,dosActivos,intro=240,pausaAnimacion,numMov;
int estadoMeta3[] = {0,1,2,3,4,5,6,7,8,9};
int estadoMeta4[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
int pasos[];
float x,y;
Random rand = new Random();
List<Integer> range;
PVector centros[][];
boolean animandoSol=false,resuelto;
Opcion bttnTamano,bttnHeuristica;
Boton bttnResolver,bttnGuardar;
Espacio espacios[][];
Tablero tablero;

boolean isWindows = System.getProperty("os.name").toLowerCase().startsWith("windows");
String directorioActual =System.getProperty("user.dir");
Process process; //<>//

void setup() {
  strokeJoin(ROUND);
  rectMode(CENTER);
  frameRate(60);
  size(800, 800);
  surface.setResizable(true);
  
  Interactive.make( this );
  bttnTamano = new Opcion( 20, height-70, 200, 40,0);
  bttnHeuristica = new Opcion(width-200-20,height-70,200,40,1);
  bttnResolver = new Boton(width/2-75, 20,150,50,"RESOLVER",31,true);
  bttnGuardar = new Boton(width-220,height-70,200,40,"     Guardar estado actual \n              como meta",14,false);
  
  Interactive.setActive(bttnTamano,false);
  Interactive.setActive(bttnHeuristica,false);
  Interactive.setActive(bttnResolver,false);
  Interactive.setActive(bttnGuardar,false);
  
  espacios = new Espacio[n][n];
  directorioActual = dataPath("");
  println(directorioActual);
}

void draw() {
  background(#AD2831);
  if(frameCount<intro){
    textAlign(CENTER,CENTER);
    textSize(100);
    if(frameCount<intro-60){
      fill(#38040E);
      text("PUZZLE",height/2,map(sin(frameCount*PI/(2*intro-120)),0,1,0,width/2));
    }
    else{
      fill(#38040E,map(frameCount,intro-50,intro-10,256,0));
      text("PUZZLE",height/2,width/2);
    }
  }
  else{
    n = bttnTamano.estado ? 3 : 4;
    if(n==3){
    } else {
    }
    if(n!=nPrev){
      animandoSol = false;
      if(n==4){
        Interactive.setActive(bttnTamano,true);
        //Interactive.setActive(bttnHeuristica,true);
        Interactive.setActive(bttnResolver,true);
        Interactive.setActive(bttnGuardar,true);
      }
      sz = floor((bttnTamano.estado ? .21 : .15)*min(height, width));
      centros = new PVector[n][n];
      
      for(int i=0;i<n;i++){
        for(int j=0;j<n;j++){
          x = width/2+(sz*.5*(bttnTamano.estado ? 0 : 1))-(sz*(n-2))+i*sz;
          y = height/2+(sz*.5*(bttnTamano.estado ? 0 : 1))-(sz*(n-2))+j*sz;
          centros[i][j] = new PVector(x,y);
        }
      }
      for(int i=0;i<nPrev;i++){
        for(int j=0;j<nPrev;j++){
          Interactive.setActive(espacios[i][j],false);
        }
      }
      espacios = new Espacio[n][n];
      /*range = new ArrayList(n*n - 1);
      for(int i = 0; i <= n*n-1; i++, range.add(i-1));
      
      for(int i=0;i<n;i++){
        for(int j=0;j<n;j++){
          a = rand.nextInt(range.size());
          b = range.get(a);
          range.remove(a);
          
          x = centros[i][j].x;
          y = centros[i][j].y;
          espacios[i][j] = new Espacio(x ,y ,b ,bttnTamano.estado);
        }*/
        for(int i=0;i<n;i++){
          for(int j=0;j<n;j++){            
            x = centros[i][j].x;
            y = centros[i][j].y;
            espacios[i][j] = new Espacio(x ,y ,i+j*n ,bttnTamano.estado);
          }
        }
      tablero = new Tablero(espacios,centros);
      if(n==3){
        estadoMeta3 = tablero.extraerEstado(3);
      } else{
        estadoMeta4 = tablero.extraerEstado(4);
      }
    }
    nPrev = n;
    
    if(bttnResolver.huboClic){
      bttnResolver.huboClic = false;
      animandoSol = true;
      if(!resuelto){
        try{
          ProcessBuilder builder = new ProcessBuilder("chmod", "755","AEstrella.lisp");
          builder = builder.directory(new File(directorioActual));
          builder.redirectErrorStream(true);
          process = builder.start();
          
          BufferedReader r = new BufferedReader(new InputStreamReader(process.getInputStream()));
          String line;
          while (true) {
              line = r.readLine();
              if (line == null) { break; }
              System.out.println(line);
          }
        } catch (Exception e) {
            println("Error, por favaro colocar el archivo: AEstrella.lisp en "+directorioActual);
            e.printStackTrace();
        }
        try{
          String estadoInicial,estadoFinal;
          if(n==4){
            estadoInicial = Arrays.toString(tablero.extraerEstado(4)).replaceAll("\\[|\\]|,|", "");
            estadoFinal = Arrays.toString(estadoMeta4).replaceAll("\\[|\\]|,|", "");
          } else{
            int[] estadoCrudo = tablero.extraerEstado(3);
            int[] inicialAux = new int[16];
            int[] finalAux = new int[16];
            for(int i=0;i<3;i++){
              for(int j=0;j<3;j++){
                inicialAux[j*4+i] = estadoCrudo[j*3+i];
                finalAux[j*4+i] = estadoMeta3[j*3+i];
              }
            }
            for(int i=0;i<4;i++){
              inicialAux[12+i] = 9+i;
              finalAux[12+i] = 9+i;
            }
            for(int j=0;j<4;j++){
              inicialAux[j*4+3] = 12+j;
              finalAux[j*4+3] = 12+j;
            }
            estadoInicial = Arrays.toString(inicialAux).replaceAll("\\[|\\]|,|", "");
            estadoFinal = Arrays.toString(finalAux).replaceAll("\\[|\\]|,|", "");
          }
          ProcessBuilder builder = new ProcessBuilder("./AEstrella.lisp",estadoInicial,estadoFinal);
          builder = builder.directory(new File(directorioActual));
          builder.redirectErrorStream(true);
          process = builder.start();
          
          BufferedReader r = new BufferedReader(new InputStreamReader(process.getInputStream()));
          String line,pasosStr="";
          while (true) {
              line = r.readLine();
              if (line == null) { break; }
              pasosStr = line;
              System.out.println(line);
          }
          String[] pasosStrSep = pasosStr.replaceAll("\\(|\\)","").split(" ");
          pasos = new int[pasosStrSep.length];
          
          int i = 0;
          for (String num : pasosStrSep){
              pasos[i++] = Integer.parseInt(num); 
          }
        } catch (Exception e) {
            println("Error, por favaro colocar el archivo: AEstrella.lisp en "+directorioActual);
            e.printStackTrace();
        }
        resuelto = true;
        numMov = 0;
        pausaAnimacion = frameCount;
      }
    }
    
    if(bttnGuardar.huboClic){
      bttnGuardar.huboClic = false;
      if(n==4){
        estadoMeta4 = tablero.extraerEstado(4);
      }
      if(n==3){
        estadoMeta3 = tablero.extraerEstado(3);
      }
    }
    
    if(!animandoSol){
      if(keyPressed){
        if(key == 'w' && frameCount>pausa[0]+17){
          pausa[0] = frameCount;
          tablero.movimiento("arriba",n);
        }
        if(key == 's' && frameCount>pausa[1]+17){
          pausa[1] = frameCount;
          tablero.movimiento("abajo",n);
        }
        if(key == 'a' && frameCount>pausa[2]+17){
          pausa[2] = frameCount;
          tablero.movimiento("izquierda",n);
        }
        if(key == 'd' && frameCount>pausa[3]+17){
          pausa[3] = frameCount;
          tablero.movimiento("derecha",n);
        }
      }
      else{
        dosActivos=0;
        for(int i=0;i<n;i++){
          for(int j=0;j<n;j++){
            if(espacios[i][j].enCambio){
              dosActivos += 1;
              if(dosActivos==1){
                a=i;
                b=j;
              }
              if(dosActivos==2){
                espacios[a][b].enCambio = false;
                espacios[i][j].enCambio = false;
                tablero.intercambiar(a,b,i,j);
                break;
              }
            }
          }
        }
      }
    }
    else{
      for(int i=0;i<n;i++){
        for(int j=0;j<n;j++){
          espacios[i][j].enCambio = false;
        }
      }
      if(frameCount > pausaAnimacion+30){
        pausaAnimacion = frameCount;
        switch(pasos[numMov]){
          case 1 : 
            tablero.movimiento("arriba",n);
            break;
          case 2 :
            tablero.movimiento("derecha",n);
            break;
          case 3 :
            tablero.movimiento("abajo",n);
            break;
          case 4 :
            tablero.movimiento("izquierda",n);
            break;
        }
        numMov++;
        if(numMov>pasos.length-1){
          resuelto = false;
          animandoSol = false;
        }
      }
    }
  }
}

public class Opcion{
    float x,y,width,height;
    boolean estado;
    int tipo,txtSz,padding1,padding2;
    String txt1,txt2;

    Opcion ( float xx, float yy, float ww, float hh, int ttipo) {
        x = xx; y = yy; width = ww; height = hh; tipo = ttipo;
        switch(tipo){
          case 0:
            txt1 = "3x3";
            txt2 = "4x4";
            txtSz = floor(height*.9);
            padding1 = 0;
            padding2 = 27;
            break;
          case 1:
            txt1 = "Manhatan";
            txt2 = "Compleja";
            txtSz = floor(height*.45);
            padding1 = 9;
            padding2 = 4;
            break;
        }
        Interactive.add( this );
    }

    void mousePressed (){
        estado = !estado;
    }

    void draw (){
      
        textAlign(LEFT,BOTTOM);
        rectMode(CORNER);
        strokeWeight(8);
        textSize(txtSz);
        
        fill( estado ? #38040E : #640D14 );
        stroke(estado ? #38040E : #640D14 );
        rect( x, y+(estado ? 0 : 4), width/2, height );
        fill( estado ? #800E13 : #AD2831 );
        text(txt1,x+4+.4*padding2,y+height+(estado ? 0 : 4)-padding1);
        
        fill( estado ? #640D14 : #38040E );
        stroke(estado ? #640D14 : #38040E);
        rect( x+width/2, y+(estado ? 4 : 0), width/2, height );
        fill( estado ? #AD2831 : #800E13 );
        text(txt2,x+4+width/2+.4*padding2,y+height+(estado ? 5 : 0)-padding1);
    }
}

public class Espacio{
  float x,y,posX,posY,width,height,sz;
  Integer id;
  boolean tres,enCambio;
  PVector activo;
  
  Espacio(float xx, float yy, int idd, boolean esTres){
    sz = (esTres ? 75 : 50);
    x = xx-sz; y = yy-sz; id = idd; tres = esTres;
    width = sz*2;
    height = sz*2;
    Interactive.add( this );
  }
  
  void mousePressed (){
      enCambio = !enCambio;
  }
  
  void draw(){
    textAlign(CENTER,CENTER);
    textSize(sz*.9);
    
    if(enCambio){stroke(#e85a5a);}
    else{stroke((id==0) ? #38040E : #640D14);}
    strokeWeight(8);
    fill((id==0) ? #38040E : #640D14);
    square(x,y,sz*2);
    noStroke();
    fill(#AD2831);
    text(id.toString(),x+sz,y+sz);
  }
}

public class Tablero{
  Espacio espacios[][];
  PVector centros[][];

  Tablero(Espacio[][] esp, PVector[][] cnt){
    espacios = esp; centros = cnt;
  }
  
  void intercambiar(int i,int j, int k, int l){
    Espacio a = espacios[i][j];
    Espacio b = espacios[k][l];
    float ax = a.x, ay = a.y;
    a.x = b.x;
    a.y = b.y;
    b.x = ax;
    b.y = ay;
    espacios[i][j] = b;
    espacios[k][l] = a;
  }
  
  void movimiento(String mov,int n){
    int x=-1,y=-1;
    for(int i=0;i<n;i++){
      for(int j=0;j<n;j++){
        if(espacios[i][j].id == 0) {
          x=i;
          y=j;
          break;
        }
      }
    }
    if(mov == "arriba" && y>0){
      intercambiar(x,y,x,y-1);
    }
    if(mov == "abajo" && y<n-1){
      intercambiar(x,y,x,y+1);
    }
    if(mov == "izquierda" && x>0){
      intercambiar(x,y,x-1,y);
    }
    if(mov == "derecha" && x<n-1){
      intercambiar(x,y,x+1,y);
    }
  }
  
  int[] extraerEstado(int n){
    int[] aux = new int[n*n];
    int k = 0;
    for(int i=0;i<n;i++){
      for(int j=0;j<n;j++){
        aux[k++] = espacios[j][i].id;
      }
    }
    return aux;
  }
}

public class Boton{
  float x,y,width,height;
  String txt;
  int txtSz;
  boolean huboClic,tipo;

  Boton( float xx, float yy, float ww, float hh, String ttxt,int ttxtSz, boolean ttipo) {
      x = xx; y = yy; width = ww; height = hh; txt = ttxt; txtSz = ttxtSz; tipo = ttipo;
      Interactive.add( this );
  }
  
  void mousePressed (){
      huboClic = true;
  }
  
  void draw(){
    textAlign(LEFT,BOTTOM);
    textSize(txtSz);
    
    if(huboClic){stroke(#e85a5a);}
    else{stroke(tipo ? #38040E : #640D14);}
    strokeWeight(8);
    fill(tipo ? #38040E : #640D14);
    rect(x,y,width,height);
    noStroke();
    fill(#AD2831);
    text(txt,x+1,y+height-(txtSz/4.4));
  }
}
