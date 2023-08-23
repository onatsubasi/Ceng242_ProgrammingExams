#include "jazz.h"
#include "metal.h"
#include "kpop.h"
#include "rock.h"

int KPopBand::play(MusicBand *other)
{
    if (get_energy() >= 0){
        int score;
        double c, k = 0.2;
        if (MusicBand* test = dynamic_cast<KPopBand*>(other)){
            c = 2.0;
        }
        else c = 0.5;
        score = (get_fan_count() + 0.1 * get_talent() * get_energy()) * c;
        set_energy( get_energy() - k*get_energy());
        return score;
    }
}

void KPopBand::rehearse(void) 
{
    if (get_energy() >= 0){
        double k = 0.2;
        int t = 0;
        set_energy( get_energy() - k*0.5*get_energy());
        set_talent(get_talent() + t);
    }
}